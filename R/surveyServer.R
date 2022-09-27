#' Server function for the survey module
#'
#' @param id The module id. Must be the same as in \code{\link{surveyUI}}.
#' @param questionFile A valid file path to an existing CSV file, formatted according to the instructions in \code{\link{surveyPrep}}. Must be the same as the one used in \code{\link{surveyUI}}.
#' @param notListedLab For non-textInput questions, the name of the choice that will allow your participants to enter their own value for all questions that include this string in the 'options' column of `questionFile`. Must be the same as the one used in \code{\link{surveyUI}}.
#' @param outFile Name of rds or csv file to create with a participant's answers. Must be unique for each participant.
#' @param returnVals Vector of inputs to return as reactive values. Must be a subset of the input IDs from the id column of `questionFile`.
#' @param result Action to perform when the participant clicks the submit button. Must be one of 'clear' (to clear the form) or 'hide' (to hide all form UI elements).
#'
#' @return A CSV or RDS file containing the values input by the participant as well as a character vector indicating the name of the file returned (same as supplied in `outFile`, unless `outFile` already exists; see note below), and/or a list of reactive values specified by `returnVals`.
#' @note This function returns a file every time the submit button is clicked. To avoid over-writing files, the function will check if the file specified in `outfile` exists. To use a custom naming scheme for individual files (e.g., to name all files from the same participant with the same prefix), we recommend pasting a unique ID number (from a reactive value) to a prefix to save a separate file for each participant. Though this function creates many small files, these files can easily be processed in bulk during analysis, which is less risky than opening and appending to existing files when multiple users visit the app simultaneously! Events can be attached to the submit button from the survey*() functions using the value of the `id` argument in the following manner: `input[["id-submit"]]`.
#' @seealso \code{\link{surveyUI}}, \code{\link{surveyPrep}}
#' @export
#' @examples
#' library(shiny)
#' library(shinyjs)
#' data("demographics")
#' write.csv(demographics, "demographics.csv", row.names = FALSE)
#' # If you use your own survey file, run surveyPrep() first!
#'
#' # You can use IDs specified in `returnVals` to trigger events after the survey is submitted.
#' if (interactive()) {
#'   ui <- fluidPage(
#'     useShinyjs(),
#'     sidebarLayout(
#'       sidebarPanel(width = 4,
#'                    textOutput("answer")),
#'       mainPanel(width = 8,
#'                 actionButton("begin", "Begin"),
#'                 surveyUI(id = "survey",
#'                          title = "Background Information Survey",
#'                          questionFile = "demographics.csv",
#'                          notListedLab = "Not listed:")
#'       )
#'     )
#'   )
#'
#'   server <- function(input, output, session) {
#'     observeEvent(input$begin, {
#'       hide("begin")
#'       # Show the survey when "begin" is clicked.
#'       answers <- surveyServer(id = "survey",
#'                               questionFile = "demographics.csv",
#'                               notListedLab = "Not listed:",
#'                               outFile = "sample.rds",
#'                               returnVals = c("age", "sex"),
#'                               result = "clear")
#'
#'       # Once an answer for "age" is submitted, show the answer in the sidebar panel.
#'       observeEvent(answers$age, {
#'         output$answer <- renderText({paste0("Your answers were saved as ", answers$filename,". You are ", answers$age, " years old.")})
#'       })
#'     })
#'   }
#'   shinyApp(ui = ui, server = server)
#' }
#'
#' # Or, you can trigger events on click of the survey's submit button using the module id in the following string: `input[["id-submit"]]`
#' if (interactive()) {
#'   ui <- fluidPage(
#'     sidebarLayout(
#'       sidebarPanel(width = 4,
#'                    textOutput("age")),
#'       mainPanel(width = 8,
#'                 actionButton("begin", "Begin"),
#'                 surveyUI(id = "survey",
#'                          title = "Background Information Survey",
#'                          questionFile = "demographics.csv",
#'                          notListedLab = "Not listed:")
#'       )
#'     )
#'   )
#'
#'   server <- function(input, output, session) {
#'     observeEvent(input$begin, {
#'       answers <- surveyServer(id = "survey",
#'                               questionFile = "demographics.csv",
#'                               notListedLab = "Not listed:",
#'                               outFile = "sample.rds",
#'                               returnVals = c("age", "sex"),
#'                               result = "hide")
#'
#'       observeEvent(input[["survey-submit"]], {
#'         output$age <- renderText({paste0("You are ", answers$age, " years old.")})
#'       })
#'     })
#'   }
#'   shinyApp(ui = ui, server = server)
#' }
surveyServer <- function (id = "survey",
                          questionFile,
                          notListedLab = NULL,
                          outFile = NULL,
                          returnVals = NULL,
                          result = c("clear","hide")){
  if (!file.exists(questionFile) | !grepl("csv$", questionFile))
    stop("Argument 'questionFile' must be a valid file path (relative to the current directory) to an existing CSV file.")
  if (!grepl("csv$", outFile) & !grepl("rds$", outFile))
    stop("Argument 'outFile' must be of the format '.csv' or '.rds'.")
  if (result != "clear" & result != "hide")
    stop("Argument result must be either 'clear' or 'hide'")
  qs <- read.csv(file = questionFile) %>% dplyr::mutate(priority = ifelse(priority ==  "required" | priority == "req", "r",
                                                                          ifelse(priority == "optional" | priority == "opt", "o",
                                                                                 as.character(priority)))) %>%
    dplyr::mutate(type = ifelse(type == "textInput" | type == "text", "t",
                                ifelse(type == "selectInput" | type ==  "select", "s",
                                       ifelse(type == "numericInput" | type == "numeric", "n",
                                              ifelse(type == "radioButtons" | type == "radio", "r",
                                                     ifelse(type == "checkboxGroupInput" | type == "checkbox", "c",
                                                            as.character(type))))))) %>%
    dplyr::mutate(across(everything(), .fns = as.character))

  if (!all(returnVals %in% qs$id))
    stop("Error: returnVals must be a character vector that contains ONLY a set of values from the 'id' column in 'questionFile', that you want returned as reactives.")

  qsl <- setNames(split(qs, seq(nrow(qs))), rownames(qs))

  mandatory <- c(qs$id[qs$priority == "r"])

  shiny::moduleServer(id = id, function(input, output, session) {
    ns <- session$ns
    returns <- shiny::reactiveValues()
    shiny::observe({
      shinyjs::showElement("survey_div")
    })
    lapply(seq_along(qsl), function(i) {
      shiny::observeEvent(input[[paste0(qsl[[i]]$id)]],
                          {
                            if (as.character(input[[paste0(qsl[[i]]$id)]]) == notListedLab) {
                              shiny::insertUI(paste0("#", ns(qsl[[i]]$id)),
                                              where = "afterEnd",
                                              ui = shiny::textInput(inputId = ns(paste0(qsl[[i]]$id, "_nl")),
                                                                    label = "Type your answer to add it to the list of choices above:"))
                            }
                          })
      shiny::observeEvent(input[[paste0(qsl[[i]]$id, "_nl")]],
                          {
                            tmp <- input[[paste0(qsl[[i]]$id, "_nl")]]
                            shiny::updateSelectInput(session, paste0(qsl[[i]]$id),
                                                     choices = tmp, selected = tmp)
                          })
    })
    shiny::observe({
      filled <- vapply(mandatory, function(x) {
        !is.null(input[[x]]) && input[[x]] != ""
      }, logical(1))
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
        if (file.exists(outFile)) {
          basen <- gsub("\\.[[:alpha:]]{3}$", "", outFile)
        } else {
          newFile <- outFile
        }
        if (grepl("rds$", outFile)) {
          for (i in 0:9999){
            if (file.exists(paste0(basen, formatC(i, width = 4,
                                           format = "d", flag = "0"),
                                   ".rds"))) {
              next
            } else {
              saveRDS(formInfo(), paste0(basen, formatC(i, width = 4,
                                                        format = "d", flag = "0"),
                                         ".rds"))
              newFile <- paste0(basen, formatC(i, width = 4,
                                               format = "d", flag = "0"),
                                ".rds")
              break
            }
          }
        } else if (grepl("csv$", outFile)) {
          for (i in 0:9999){
            if (file.exists(paste0(basen,
                                   formatC(i, width = 4,
                                           format = "d", flag = "0"),
                                   ".csv"))) {
              next
            } else {
              write.csv(formInfo(),
                        paste0(basen, formatC(i, width = 4,
                                              format = "d", flag = "0"),
                               ".csv"), row.names = FALSE)
              newFile <- paste0(basen, formatC(i, width = 4,
                                               format = "d", flag = "0"),
                                ".csv")
              break
            }
          }
        }
      }

      if (result == "hide") {
        shinyjs::disable("submit")
        shinyjs::hide("survey_div")
      }
      else if (result == "clear") {
        shinyjs::disable("submit")
        shinyjs::reset("survey_div")
      }

      if (length(returnVals) > 0) {
        for (i in 1:length(returnVals)) {
          returns[[returnVals[i]]] <- input[[paste0(returnVals[i])]]
        }
      }

      returns[["filename"]] <- newFile
    })
    return(returns)
  })
}

