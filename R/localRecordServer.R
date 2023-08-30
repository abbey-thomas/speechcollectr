#' Server function for Recording User Audio
#'
#' @description Same as `recordServer()`, but faster (suited to slow internet connections). The server function for recording user audio (using local scripts) enables the 'stop' button after the user begins recording and checks to make sure the user has given the website permission to record audio in their browser. Requires the UI \code{\link{localRecordUI}}.
#' @param id The input ID associated with the record module. Must be the same as the id of `localRecordUI()`.
#' @param trigger A reactive expression (i.e., something wrapped in "shiny::reactive()") that will trigger the recording interface to appear and run. Default is "NULL", so the module will only run (and output a uniquely named file) each time the user clicks start. If using the default NULL trigger, you must call "shinyjs::showElement()" to make the recording interface appear!
#' @param outFile Character or reactive expression. Where to store the audio file. Can indicate any subdirectory of the present working directory. If dynamic, wrap in `reactive()`.
#' @param Overwrite Boolean. Defaults to `FALSE` so that a unique digit is appended to each filename so that all recordings will be saved even when the filename value is the same. If `TRUE`, will overwrite a file of the same name.
#' @param writtenStim Either a character vector (for a single, static stimulus) or a reactive expression (created with reactive, for a stimulus that should be updated from trial to trial) representing a written stimulus that a participant will read while recording.
#' @param writtenDelay Integer. How many milliseconds should elapse between the time the participant clicks `record` and the time the written stimulus appears? Defaults to 500. We recommend not using a value less than that.
#'
#' @return In the case of a NULL trigger, returns a reactive expression containing two values: (1) `n`: the number of files recorded and (2) `file`: a character vector with length `n`, where each element is the name of a recorded wav file. If `trigger` argument is not NULL, returns a reactive expression containing: (1) `n`: the number of attempts at recording the current file and (2) `file`: the filename where only the most recent attempt is saved (previous attempts have been overwritten). Also returns a 16 bit, 44.1 kHz wav file in a filename comprising `outPrefix` and a unique four digit number.
#' @note The "start" and "stop" buttons from this module can be accessed in the server code with the "id" of the module as follows: `input[["id-start"]]` or `input[["id-stop"]]`
#' @family Audio recording module
#' @seealso Must be used with \code{\link{localRecordUI}}.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   library(shinyjs)
#'   ui <- fluidPage(
#'     actionButton("begin", "BEGIN"),
#'     localRecordUI(id ="rec")
#'   )
#'
#'   # Using the default "NULL" trigger argument, the record server function will save
#'   # a new file every time the participant clicks start,
#'   # with a new name generated each time a participant clicks start.
#'   server <- function(input, output, session) {
#'     recording <- localRecordServer(id ="rec",
#'                           outPrefix = paste0("www/rec_samp"))
#'
#'     # You'll need to make the recording interface visible though!
#'     # Which can be accomplished with shinyjs::showElement()
#'     observeEvent(input$begin, {
#'       showElement("rec")
#'     })
#'   }
#'  shinyApp(ui = ui, server = server)
#' }
#'
#' # Alternatively an explicit trigger argument can be set as shown below
#' if (interactive()) {
#'   library(shiny)
#'   library(shinyjs)
#'
#'   ui <- fluidPage(
#'     actionButton("begin", "BEGIN"),
#'     localRecordUI(id ="rec")
#'   )
#'
#'   # Now a new filename will be used if and only if the participant clicks "begin"
#'   # Otherwise, the same file will be over-written
#'   # This is useful if we want to give a participant multiple attempts at recording...
#'   server <- function(input, output, session) {
#'     recording <- localRecordServer(id ="rec",
#'                           trigger = reactive(input$begin),
#'                           outPrefix = paste0("www/rec_samp"))
#'
#'     # But let's say we don't want them to have more than 3 attempts to record the file
#'     # We can do this by attaching an event to the "stop" button of the recorder,
#'     # which is saved in the apps input list as "(id)-stop"
#'     observeEvent(input[["rec-stop"]], {
#'       if (recording()$n == 3) {
#'         hide("rec")
#'       }
#'     })
#'   }
#'  shinyApp(ui = ui, server = server)
#' }
localRecordServer <- function (id = "record", trigger = NULL, outFile, overwrite = FALSE,
                          writtenStim = NULL, writtenDelay = 500)
{
  session <- shiny::getDefaultReactiveDomain()
  record_rvs <- shiny::reactiveValues(n = 0, played = 0)
  if (is.null(trigger)) {
    shiny::observeEvent(session$input[[paste0(id, "-start")]],
                        {
                          shinyjs::disable(paste0(id, "-start"))
                          record_rvs$n <- record_rvs$n + 1
                          if (isTRUE(overwrite)) {
                            record_rvs$filepath <- ifelse(!shiny::is.reactive(outFile),
                                                          paste0(outFile), paste0(outFile()))
                          }
                          else {
                            record_rvs$outPrefix <- ifelse(!shiny::is.reactive(outFile),
                                                           paste0(outFile), paste0(outFile()))
                            record_rvs$outPrefix <- gsub("\\.wav$", "",
                                                         record_rvs$outPrefix)
                            file_list <- list.files(path = gsub("/[^/]+?$",
                                                                "", record_rvs$outPrefix), pattern = paste0(gsub(".*/",
                                                                                                                 "", record_rvs$outPrefix), ".*", "\\.wav$"),
                                                    full.names = TRUE)
                            file_nums <- gsub(paste0(record_rvs$outPrefix),
                                              "", file_list)
                            file_nums <- gsub("\\.wav", "", file_nums)
                            file_nums <- as.numeric(file_nums[grepl("\\d{3}",
                                                                    file_nums)])
                            file_num <- ifelse(length(file_nums) > 0,
                                               max(file_nums) + 1, 0)
                            record_rvs$filepath <- paste0(record_rvs$outPrefix,
                                                          formatC(file_num, width = 3, format = "d",
                                                                  flag = "0"), ".wav")
                          }
                        })
  }
  else {
    shiny::observeEvent(trigger(), {
      shinyjs::showElement(paste0(id))
      record_rvs$attempt <- 0
      if (isTRUE(overwrite)) {
        record_rvs$filepath <- ifelse(!shiny::is.reactive(outFile),
                                      paste0(outFile), paste0(outFile()))
      }
      else {
        record_rvs$outPrefix <- ifelse(!shiny::is.reactive(outFile),
                                       paste0(outFile), paste0(outFile()))
        record_rvs$outPrefix <- gsub("\\.wav$", "",
                                     record_rvs$outPrefix)
        file_list <- list.files(path = gsub("/[^/]+?$",
                                            "", record_rvs$outPrefix), pattern = paste0(gsub(".*/",
                                                                                             "", record_rvs$outPrefix), ".*", "\\.wav$"),
                                full.names = TRUE)
        file_nums <- gsub(paste0(record_rvs$outPrefix),
                          "", file_list)
        file_nums <- gsub("\\.wav", "", file_nums)
        file_nums <- as.numeric(file_nums[grepl("\\d{3}",
                                                file_nums)])
        file_num <- ifelse(length(file_nums) > 0, max(file_nums) +
                             1, 0)
        record_rvs$filepath <- paste0(record_rvs$outPrefix,
                                      formatC(file_num, width = 3, format = "d",
                                              flag = "0"), ".wav")
      }
    })
    shiny::observeEvent(session$input[[paste0(id, "-start")]],
                        {
                          shinyjs::disable(paste0(id, "-start"))
                          record_rvs$n <- record_rvs$n + 1
                          record_rvs$attempt <- record_rvs$attempt + 1
                        })
  }
  shiny::observeEvent(record_rvs$filepath, {
    r_file <- record_rvs$filepath
    shiny::observeEvent(session$input[["ready"]],
                        {
                          shinyjs::delay(500, shinyjs::enable(paste0(id,
                                                                     "-stop")))
                          if (!is.null(writtenStim)) {
                            if (is.vector(writtenStim)) {
                              session$output[[paste0(id, "-stim")]] <- shiny::renderText({
                                as.character(writtenStim)
                              })
                            }
                            else {
                              session$output[[paste0(id, "-stim")]] <- shiny::renderText({
                                as.character(writtenStim())
                              })
                            }
                            shinyjs::delay(as.numeric(writtenDelay), shinyjs::showElement(paste0(id,
                                                                                                 "-stimDiv")))
                          }
                        })
    shiny::observeEvent(session$input$audioOut, {
      audio <- session$input$audioOut
      audio <- gsub("data:audio/wav;base64,", "", audio)
      audio <- gsub(" ", "+", audio)
      audio <- RCurl::base64Decode(audio, mode = "raw")
      inFile <- file(record_rvs$filepath, "wb")
      writeBin(audio, inFile)
      close(inFile)
    })
  })
  shiny::observeEvent(session$input[[paste0(id, "-stop")]],
                      {
                        shinyjs::disable(paste0(id, "-stop"))
                        if (!is.null(writtenStim)) {
                          shinyjs::hide(paste0(id, "-stimDiv"))
                        }
                        shinyjs::delay(500, shinyjs::enable(paste0(id, "-start")))
                        shinyjs::delay(500, record_rvs$check <- TRUE)
                      })

  retval <- shiny::eventReactive(session$input[[paste0(id,
                                                       "-stop")]], {
                                                         if (is.null(trigger)) {
                                                           return(list(n = record_rvs$n, file = record_rvs$filepath))
                                                         }
                                                         else {
                                                           return(list(n = record_rvs$attempt, file = record_rvs$filepath))
                                                         }
                                                       })
  return(retval)
}
