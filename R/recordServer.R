#' Server function for Recording User Audio
#'
#' @description The server function for recording user audio enables the 'stop' button after the user begins recording and checks to make sure the user has given the website permission to record audio in their browser. Requires the UI \code{\link{recordUI}}.
#' @param id The input ID associated with the record module. Must be the same as the id of `recordUI()`.
#' @param outFile Character or reactive expression. Where to store the audio file. Can indicate any subdirectory of the present working directory. If dynamic, wrap in `reactive()`.
#' @param attempts Numeric (Defaults to Inf). How many attempts to create this recording should the participant be allowed?
#' @param overwrite Boolean. Defaults to `FALSE` so that a unique digit is appended to each filename so that all recordings will be saved even when the filename value is the same. If `TRUE`, will overwrite a file of the same name.
#' @param writtenStim Either a character vector (for a single, static stimulus) or a reactive expression (created with reactive, for a stimulus that should be updated from trial to trial) representing a written stimulus that a participant will read while recording.
#' @param writtenDelay Integer. How many milliseconds should elapse between the time the participant clicks `record` and the time the written stimulus appears? Defaults to 500. We recommend not using a value less than that.
#'
#' @return Returns a reactive expression containing: (1) `n`: the number of attempts at recording the current file and (2) `file`: the filename where only the most recent attempt is saved (previous attempts have been overwritten). Also returns a wav file in a filename comprising `outPrefix` and a unique four digit number.
#' @note Must be placed at the top level of the application's server function. The "start" and "stop" buttons from this module can be accessed in the server code with the "id" of the module as follows: `input[["id-start"]]` or `input[["id-stop"]]`. The submit button can be accessed with `input[["id-submit"]]`.
#' @family Audio recording module
#' @seealso Must be used with \code{\link{recordUI}}. For a more flexible audio recording protocol, see \code{\link{startRec}}, \code{\link{stopRec}}, and \code{\link{useRecorder}}.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   library(shinyjs)
#'
#'   # Build UI
#'   ui <- fluidPage(
#'
#'     ## Get background javascript ready
#'     useShinyjs(),
#'
#'     ## Setup page layout
#'     div(id = "trialDiv",
#'         style = "text-align:center;",
#'         actionButton("next_trial", "NEXT"),
#'
#'         ### Initialize the recording interface invisibly
#'         hidden(recordUI(id = "rec_module"))
#'     )
#'   )
#'
#'   # The Server function
#'   server <- function(input, output, session) {
#'     ## Create an object that will count trials
#'     rvs <- reactiveValues(trial_n = 0)
#'
#'     ## When the participant clicks "NEXT"...
#'     observeEvent(input$next_trial, {
#'       ### Increase the trial number
#'       rvs$trial_n <- rvs$trial_n + 1
#'
#'       ### Show the recording interface
#'       showElement("rec_module")
#'
#'       ### Hide the next button
#'       hide("next_trial")
#'     })
#'
#'     ## Note that the call to recordServer() is at the top level of our app's server function
#'     ## And the returned filename and text to be read are wrapped in reactive()
#'     ## (since they need to be reactive).
#'     recordServer(id = "rec_module",
#'                  attempts = 3, overwrite = TRUE,
#'                  outFile = reactive(paste0("rec", rvs$trial_n, ".wav")),
#'                  writtenStim = reactive(paste0("This is recording ", rvs$trial_n, ".")))
#'
#'     ## As with all speechcollectr modules, the submit button's Id
#'     ## can be accessed with the module id + `-submit`
#'     ## Here when the user clicks submit inside the recording module...
#'     observeEvent(input[["rec_module-submit"]], {
#'
#'       ## Make the next button reappear
#'       showElement("next_trial")
#'     })
#'
#'   }
#'   # Run the application
#'   shinyApp(ui = ui, server = server)
#' }
#'
recordServer <- function(id = "record",
                         outFile,
                         attempts = Inf,
                         overwrite = FALSE,
                         writtenStim = NULL,
                         writtenDelay = 500) {
  session <- shiny::getDefaultReactiveDomain()

  record_rvs <- shiny::reactiveValues(n = 0)

  shiny::observeEvent(session$input[[paste0(id, "-start")]], {
    record_rvs$n <- record_rvs$n+1

    shinyjs::disable(paste0(id, "-start"))

    if (isTRUE(overwrite)) {
      record_rvs$filepath <- ifelse(!shiny::is.reactive(outFile),
                                    paste0(outFile), paste0(outFile()))
    } else {
      record_rvs$outPrefix <- ifelse(!shiny::is.reactive(outFile),
                                     paste0(outFile), paste0(outFile()))
      record_rvs$outPrefix <- gsub("\\.wav$", "", record_rvs$outPrefix)

      file_list <- list.files(path = gsub("/[^/]+?$", "", record_rvs$outPrefix),
                              pattern = paste0(gsub(".*/", "", record_rvs$outPrefix), ".*", "\\.wav$"),
                              full.names = TRUE)

      file_nums <- gsub(paste0(record_rvs$outPrefix, "_"), "", file_list)
      file_nums <- gsub("\\.wav", "", file_nums)
      file_nums <- as.numeric(file_nums[grepl("\\d{2}", file_nums)])
      file_num <- ifelse(length(file_nums) > 0, max(file_nums) + 1, 0)
      record_rvs$filepath <- paste0(record_rvs$outPrefix, "_",
                                    formatC(file_num, width = 2,
                                            format = "d", flag = "0"),
                                    ".wav")
    }
  })

  shiny::observeEvent(session$input[[paste0(id, "-start")]], {
    startRec()

    shiny::observeEvent(session$input[["rec-ready"]], {
      shinyjs::delay(500, shinyjs::enable(paste0(id, "-stop")))

      if (!is.null(writtenStim)) {
        if (is.vector(writtenStim)) {
          session$output[[paste0(id, "-stim")]] <- shiny::renderText({as.character(writtenStim)})
        } else {
          session$output[[paste0(id, "-stim")]] <- shiny::renderText({as.character(writtenStim())})
        }

        shinyjs::delay(as.numeric(writtenDelay),
                       shinyjs::showElement(paste0(id, "-stimDiv")))
      }
    })
  }, priority = -1)

  shiny::observeEvent(session$input[[paste0(id, "-stop")]], {
    shinyjs::enable(paste0(id, "-submit"))
    shinyjs::disable(paste0(id, "-stop"))
    stopRec(filename = paste0(record_rvs$filepath))

    if (!is.null(writtenStim)) {
      shinyjs::hide(paste0(id, "-stimDiv"))
    }

    if (record_rvs$n < attempts) {
      if (record_rvs$n == (attempts-1)) {
        shiny::showModal(shiny::modalDialog(
          shiny::HTML("<p style:'18px'> You have 1 attempt remaining for this recording.</p>"),
          footer = shiny::modalButton("Okay")
        ))
      }
      shinyjs::delay(500, shinyjs::enable(paste0(id, "-start")))
    } else {
      shiny::showModal(shiny::modalDialog(
        shiny::HTML("<p style:'18px'> You have used the maximum number of attempts for this recording. Please click the SUBMIT RECORDING button to submit your most recent attempt.</p>"),
        footer = shiny::modalButton("Okay")
      ))
    }

  })

  shiny::observeEvent(session$input[[paste0(id, "-submit")]], {
    shinyjs::disable(paste0(id, "-submit"))
    shinyjs::hide(paste0(id))
    record_rvs$n <- 0
  })

  retval <- shiny::eventReactive(session$input[[paste0(id, "-stop")]], {
    return(list(n = shiny::isolate(record_rvs$n),
                file = shiny::isolate(record_rvs$filepath)))
  })

  return(retval)
}
