#' Check that participants meet inclusion criteria, or get information about their current environment
#'
#' @description Present participants with a set of TRUE/FALSE questions one at a time. These are commonly used to check that participants meet inclusion criteria or get information about their current environment. It is recommended that experimenters confirm that participants meet qualifications before doing any data collection.
#'
#' @param id The module ID. Must be the same as the ID of `checkUI`.
#' @param trigger A reactive value indicating the event that should trigger the appearance of the consent form. May be an `input$...` value from outside the module wrapped in `reactive()`.
#' @param questionFile A .csv file containing ALL and ONLY the following columns: \emph{id, priority, label, error}. See Details below for more information.
#' @param outFile An optional file name for the (optional) return file where the participant's T/F answers will be saved.
#' @param returnVals A list of input IDs from the column "id" in `questionFile` specifying the answers you want the function to output.
#'
#' @details The questionFile argument is the source for the questions that should be displayed to the participant as statements or questions to which they must answer TRUE or FALSE. We have packaged some generic sets of questions with speechcollectr. You may access them using `data("qualifications")` or `data("environment")`. Any question file must contain ONLY the following columns:
#' \itemize{
#' \item{"id:"}{Gives a unique input ID for each question. Values MUST be unique for each row and follow the same rules as other Shiny input IDs (no spaces).}
#' \item{"label:"}{Contains the labels (i.e., the questions or statements) that will be displayed to participants.}
#' \item{"priority:"}{All items in the `check` module require an answer, but this column specifies whether the question on each row requires an answer of TRUE. There are two accepted options (required and optional), denoted by the following values (values separated by pipe (|) are treated as equivalent): 'required'|'req'|'r' and 'optional'|'opt'|'o'.}
#' \item{"error:"} {This column is required if ANY value in `priority` is equal to "required". This is the error message the participant will see when they answer FALSE to a question that requires an answer of TRUE. }
#' }
#' @return Answers that the participant gave to the questions whose input IDs are listed in the argument `returnVals`.
#'
#' @examples
#' # First get some sample questions for your participant.
#' data("qualifications")
#' write.csv(qualifications, "qualifications.csv", row.names = FALSE)
#'
#' # Now ask the questions!
#' if (interactive()) {
#'   shinyApp(
#'     ui = fluidPage(
#'       fluidRow(
#'         column(width = 8, offset = 2,
#'                actionButton("btn", "Click me"),
#'                checkUI(id = "example", title = "Speech Experiment",
#'                        type = "participant"),
#'                textOutput("confirmation"))
#'       )
#'
#'     ),
#'     server = function(input, output, session) {
#'       answer <- checkServer(id = "example",
#'                             trigger = reactive(input$btn),
#'                             questionFile = "qualifications.csv",
#'                             outFile = NULL,
#'                             returnVals = c("eighteen"))
#'       observeEvent(input$btn, {
#'         shinyjs::hide("btn")
#'       })
#'       observe({
#'         if (isTruthy(answer$eighteen))
#'           output$confirmation <- renderText("This participant is an adult.")
#'       })
#'     }
#'   )
#' }
checkServer <- function(id = "check",
                        trigger,
                        questionFile,
                        outFile = NULL,
                        returnVals) {
  if (!file.exists(questionFile)|!grepl("csv$", questionFile))
    stop("Argument 'questionFile' must be a valid file path (relative to the current directory) to an existing CSV file.")
  if (!is.null(outFile)) {
    if (!grepl("csv$", outFile) & !grepl("rds$", outFile))
      stop("Argument 'outFile' must be of the format '.csv' or '.rds'.")
  }

  qs <- read.csv(file = questionFile) %>%
    dplyr::mutate(priority = ifelse(priority == "required"|priority == "req", "r",
                                    ifelse(priority == "optional"|priority == "opt", "o",
                                           as.character(priority)))) %>%
    dplyr::mutate(across(everything(), .fns = as.character)) %>%
    tibble::rownames_to_column() %>%
    dplyr::mutate(label = paste0("(", rowname, " of ", nrow(.), ") ", label))

  nr <- nrow(qs)

  if (!all(returnVals %in% qs$id))
    stop("Error: returnVals must be a character vector that contains ONLY a set of values from the 'id' column in 'questionFile', that you want returned as reactives.")

  shiny::moduleServer(
    id=id,
    function(input, output, session){
      ns <- session$ns
      rvs <- shiny::reactiveValues(n = 1, attempt = 1)
      returns <- shiny::reactiveValues()

      shiny::observeEvent(trigger(), {
        shinyjs::showElement("ck_div")
        output$ck_qs <- shiny::renderUI({
          shinyWidgets::radioGroupButtons(inputId = ns(paste0(qs$id[rvs$n])),
                                          label = paste0(qs$label[rvs$n]),
                                          choiceNames = c("TRUE", "FALSE"),
                                          choiceValues = c(1,0),
                                          checkIcon = list(yes = shiny::icon("check")),
                                          selected = character())
        })
      })

      formInfo <- shiny::reactive({
        info <- sapply(c(qs$id), function(x) input[[x]])
        names(info) <- c(qs$id)
        info <- t(info)
        info
      })

      shiny::observeEvent(input[[paste0(qs$id[rvs$n])]], {
        if (qs$id[rvs$n] %in% returnVals){
          returns[[qs$id[rvs$n]]] <- input[[paste0(qs$id[rvs$n])]]
        }

        if (qs$priority[rvs$n] == "r" & input[[paste0(qs$id[rvs$n])]]=="0") {
          if (rvs$attempt == 1) {
            shinyjs::hide("ck_div")
            shinyalert::shinyalert(type = "info",
                                   text = qs$error[rvs$n],
                                   showCancelButton = TRUE,
                                   cancelButtonText = "EXIT STUDY",
                                   confirmButtonText = "Cancel",
                                   inputId = "mistake")
          }
        }

        if (rvs$n <= nr & input[[paste0(qs$id[rvs$n])]] == "1") {
          rvs$n <- rvs$n + 1
          rvs$attempt <- 1
          shinyjs::delay(500, {
            output$ck_qs <- shiny::renderUI({
              shinyWidgets::radioGroupButtons(inputId = ns(paste0(qs$id[rvs$n])),
                                              label = paste0(qs$label[rvs$n]),
                                              choiceNames = c("TRUE", "FALSE"),
                                              choiceValues = c(1,0),
                                              checkIcon = list(yes = shiny::icon("check")),
                                              selected = character())
            })
          })
        }
        if (rvs$n > nr) {
          shinyjs::hide("ck_div")

          if (!is.null(outFile)) {
            if (grepl("rds$", outFile)) {
              saveRDS(formInfo(), outFile)
            } else if (grepl("csv$", outFile)) {
              write.csv(formInfo(), outFile, row.names = FALSE)
            }
          }
        }
      })

      shiny::observeEvent(input$mistake, {
        if (input$mistake != 0){
          rvs$attempt <- 2
          shinyWidgets::updateRadioGroupButtons(session, inputId = paste0(qs$id[rvs$n]),
                                          label = paste0(qs$label[rvs$n]),
                                          choiceNames = c("TRUE", "FALSE"),
                                          choiceValues = c(1,0),
                                          checkIcon = list(yes = shiny::icon("check")),
                                          selected = character())
          shinyjs::showElement("ck_div")
        }
      })
      return(returns)
    }
  )
}
