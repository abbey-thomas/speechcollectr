#' Server function for the survey module
#'
#' @param id The module id. Must be the same as \code{\link{surveyUI}}.
#' @param questionFile A valid file path to an existing CSV file, formatted according to the instructions in \code{\link{surveyPrep}}. Must be the same as the one used in \code{\link{surveyUI}}.
#' @param notListedLab For non-textInput questions, the name of the choice that will allow your participants to enter their own value for all questions that include this string in the 'options' column of `questionFile`. Must be the same as the one used in \code{\link{surveyUI}}.
#' @param outFile Name of rds or csv file to create with a participant's answers. Must be unique for each participant.
#' @param returnVals Vector of inputs to return as reactive values. Must be a subset of the input IDs from the id column of `questionFile`.
#' @param trigger A reactive value indicating the event that should trigger the appearance of the consent form. May be an `input$...` value from outside the module wrapped in `reactive()`.
#' @param result Action to perform when the particpant clicks the submit button. Must be one of 'clear' (to clear the form) or 'hide' (to hide all form UI elements).
#'
#' @return A CSV or RDS file containing the values input by the participant, and/or a list of reactive values specified by `returnVals`.
#' @note This function returns a file every time the submit button is clicked. Therefore, the `outFile` argument should be unique for each run of the function. We recommend pasting a unique ID number (perhaps from a reactive value) to a prefix to save a separate file for each participant. Though this creates many small files, these files can easily be processed in bulk during analysis, which is less risky than opening and appending to existing files when multiple users visit the app simultaneously!
#' @seealso \code{\link{surveyUI}}, \code{\link{surveyPrep}}
#'
#' @examples
#' library(shiny)
#' data("demographics")
#' write.csv(demographics, "demographics.csv", row.names = FALSE)
#' # If you use your own survey file, run surveyPrep() first!
#'
#' ui <- fluidPage(
#'   sidebarLayout(
#'     sidebarPanel(width = 4,
#'                  textOutput("age")),
#'     mainPanel(width = 8,
#'               actionButton("begin", "Begin"),
#'               surveyUI(id = "survey",
#'                        title = "Background Information Survey",
#'                        questionFile = "demographics.csv",
#'                        notListedLab = "Not listed:")
#'     )
#'   )
#' )
#' server <- function(input, output, session) {
#'   answers <- surveyServer(id = "survey",
#'                questionFile = questionFile,
#'                notListedLab = "Not listed:",
#'                outFile = "sample.rds",
#'                trigger = reactive(input$begin),
#'                returnVals = c("age", "sex"),
#'                result = "clear")
#'   observeEvent(answers$age, {
#'     output$age <- renderText({paste0("You are ", answers$age, " years old.")})
#'   })
#' }
#' shinyApp(ui = ui, server = server)
surveyServer <- function(id = "survey",
                         questionFile,
                         notListedLab = NULL,
                         outFile = NULL,
                         returnVals = NULL,
                         trigger,
                         result = c("clear","hide")) {

  if (!file.exists(questionFile)|!grepl("csv$", questionFile))
    stop("Argument 'questionFile' must be a valid file path (relative to the current directory) to an existing CSV file.")
  if (!grepl("csv$", outFile) & !grepl("rds$", outFile))
    stop("Argument 'outfile' must be of the format '.csv' or '.rds'.")
  if (result != "clear" & result != "hide")
    stop("Argument result must be either 'clear' or 'hide'")

  qs <- read.csv(file = questionFile) %>%
    dplyr::mutate(priority = ifelse(priority == "required"|priority == "req", "r",
                                    ifelse(priority == "optional"|priority == "opt", "o",
                                           as.character(priority)))) %>%
    dplyr::mutate(type = ifelse(type == "textInput"|type == "text", "t",
                                ifelse(type == "selectInput"|type == "select", "s",
                                       ifelse(type == "numericInput"|type == "numeric", "n",
                                              ifelse(type == "radioButtons"|type == "radio", "r",
                                                     ifelse(type == "checkboxGroupInput"|type == "checkbox", "c",
                                                            as.character(type))))))) %>%
    dplyr::mutate(across(everything(), .fns = as.character))

  if (!all(returnVals %in% qs$id))
    stop("Error: returnVals must be a character vector that contains ONLY a set of values from the 'id' column in 'questionFile', that you want returned as reactives.")

  qsl <- setNames(split(qs, seq(nrow(qs))), rownames(qs))

  mandatory <- c(qs$id[qs$priority == "r"])

  shiny::moduleServer(
    id=id,

    function(input, output, session) {
      ns <- session$ns

      returns <- shiny::reactiveValues()

      observeEvent(trigger(), {
        shinyjs::showElement("survey_div")
      })

      lapply(seq_along(qsl), function(i){
        shiny::observeEvent(input[[paste0(qsl[[i]]$id)]], {
          if (as.character(input[[paste0(qsl[[i]]$id)]]) == notListedLab) {
            shiny::insertUI(paste0("#", ns(qsl[[i]]$id)),
                            where = "afterEnd",
                            ui = shiny::textInput(
                              inputId = ns(paste0(qsl[[i]]$id, "_nl")),
                              label = "Type your answer to add it to the list of choices above:"))
          }
        })

        shiny::observeEvent(input[[paste0(qsl[[i]]$id, "_nl")]], {
          tmp <- input[[paste0(qsl[[i]]$id, "_nl")]]
          shiny::updateSelectInput(session, paste0(qsl[[i]]$id),
                            choices = tmp,
                            selected = tmp)
        })
      })

      shiny::observe({
        filled <-
          vapply(mandatory,
                 function(x) {
                   !is.null(input[[x]]) && input[[x]] != ""
                 },
                 logical(1))
        filled <- all(filled)

        shinyjs::toggleState(id = "submit", condition = filled)

      })

      formInfo <- shiny::reactive({
        info <- sapply(c(qs$id), function(x) input[[x]])
        names(info) <- c(qs$id)
        info <- t(info)
        info
      })

      shiny::observeEvent(input$submit, {
        if (!is.null(outFile)) {
          if (grepl("rds$", outFile)) {
            saveRDS(formInfo(), outFile)
          } else if (grepl("csv$", outFile)) {
            write.csv(formInfo(), outFile, row.names = FALSE)
          }
        }

        if (result == "hide") {
          shinyjs::hide("survey_div")
        } else if (result == "clear") {
          shinyjs::disable("submit")
          shinyjs::reset("survey_div")
        }

        for (i in 1:length(returnVals)) {
          returns[[returnVals[i]]] <- input[[paste0(returnVals[i])]]
        }

      })
      return(returns)
    }
  )
}
