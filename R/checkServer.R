checkServer <- function(id = "check",
                        trigger,
                        questionFile,
                        outFile,
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
