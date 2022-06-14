#' Server function for Recording User Audio
#'
#' @description The server function for recording user audio enables the 'stop' button after the user begins recording and checks to make sure the user has given the website permission to record audio in their browser. Requires the UI \code{\link{recordUI}}.
#' @param id The input ID associated with the record module. Must be the same as the id of `recordUI()`.
#' @param trigger A reactive value indicating the event that should trigger the appearance of the consent form. May be an `input$...` value from outside the module wrapped in `reactive()`.
#' @param folder Character. Where to store the audio file. Defaults to the path returned by `getwd()`
#' @param filename Required. Character. The name of the file to be recorded. Do not specify a directory here. Instead, do that with the `folder` argument.
#' @param writtenStim A character vector that you want a participant to read while recording.
#' @param writtenDelay Integer. How many milliseconds should elapse between the time the participant clicks `record` and the time the written stimulus appears? Defaults to 500. We recommend not using a value less than that.
#' @param playback Boolean. Should the participant be allowed to listen to the recording before submitting? Defaults to `FALSE`.
#' @param onFail If eval=TRUE, what kind of message should the user receive if they try to record the maximum number of `tries` and the quality is still poor? Must be either "stop" (user gets an error message) or "continue" (user gets a success message and we ignore recording quality).
#'
#' @return Saves a single-channel, 16-bit, 44.1 kHz sample rate WAV file with the specified `filename` in a directory specified by `folder`. If eval==TRUE, returns a pop-up telling the user how to improve their recording quality if it is not sufficient.
#' @family Audio recording module
#' @seealso Must be used with \code{\link{recordUI}}.
#' @export
#' @examples
#' if (interactive()) {
#' ui <- shiny::fluidPage(
#'   recordUI(id = "record")
#' )
#'
#' server <- function(input, output, session) {
#'  recordServer(id = "record", filename = "sample.wav")
#' }
#' shiny::shinyApp(ui = ui, server = server)
#' }
#'
# At the end of this process which button should be enabled?
# How can users add other functions to the buttons in the module?
recordServer <- function(id = "recorder",
                         trigger,
                         folder = ".", filename,
                         writtenStim = NULL,
                         writtenDelay = 500,
                         playback = FALSE) {

  if (length(filename) == 0) stop("You must enter a filename for the recorded sound!")
  if (folder == "."){folder <- getwd()}
  shiny::moduleServer(
    id = id,

    function(input, output, session, mod_id = id) {
      ns <- session$ns
      feedback <- shiny::reactiveValues(result = 0)
      shiny::observeEvent(trigger(), {
        shinyjs::showElement("rec")
      })

      shiny::observe({
        shiny::observe(
          js$webAudioRecorder(ets ="wav",
                              mod_id = paste0(id))
        )
      })

      shiny::observeEvent(input$start, {
        shinyjs::disable("start")

        shiny::observeEvent(input$ready, {
          shinyjs::delay(500, shinyjs::enable("stop"))
          if (!is.null(writtenStim)) {
            output$stim <- shiny::renderText(as.character(writtenStim))
            shinyjs::delay(as.numeric(writtenDelay),
                           shinyjs::showElement("stim_div"))
          }
        })

        shiny::observeEvent(input$audio,
                     {
                       audio <- input$audio
                       audio <- gsub('data:audio/wav;base64,', '', audio)
                       audio <- gsub(' ', '+', audio)
                       audio <- base64enc::base64decode(audio)

                       # Save to file on server.
                       inFile <- list()
                       inFile$datapath <- file.path(folder, filename)
                       inFile$file <- file(inFile$datapath, 'wb')
                       writeBin(audio, inFile$file)
                       close(inFile$file)
                     })
      })

      shiny::observeEvent(input$stop, {
        shinyjs::disable("stop")
        shinyjs::hide("stim_div")

        if (isTRUE(playback)) {
          output$submission <- shiny::renderUI({

            playBttn(inputId = ns("replay"),
                     label = "Listen to your recording.",
                     src = file.path(folder, filename),
                     audioId = "recording",
                     fill = "white", text = "black")

            shiny::tags$br()
          })
        }

        shinyjs::delay(500, shinyjs::enable("start"))
        })

      return(shiny::reactive(feedback$result))
    }
  )
}
