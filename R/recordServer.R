#' Server function for Recording User Audio
#'
#' @description The server function for recording user audio enables the 'stop' button after the user begins recording and checks to make sure the user has given the website permission to record audio in their browser. Requires the UI \code{\link{recordUI}}.
#' @param id The input ID associated with the record module. Must be the same as the id of `recordUI()`.
#' @param folder Character. Where to store the audio file. Defaults to the path returned by `getwd()`
#' @param filename Required. Character. The name of the file to be recorded. Do not specify a directory here. Instead, do that with the `folder` argument.
#' @param eval Boolean. Should we evaluate the recording for clipping and SNR, and give the user tips on how to improve if the recording quality is poor? Defaults to FALSE.
#' @param tries Integer. If eval=TRUE, how many tries should the user get to improve their SNR and reduce clipping?
#' @param onFail If eval=TRUE, what kind of message should the user receive if they try to record the maximum number of `tries` and the quality is still poor? Must be either "stop" (user gets an error message) or "continue" (user gets a success message and we ignore recording quality).
#' @param snr_best Integer. If eval=TRUE, what is the minimum SNR (dB) required for the recording to be considered of the best quality? Defaults to 15.
#' @param snr_good Integer. If eval=TRUE, what is the minimum SNR (dB) required for the recording to be considered acceptable? Must be less than `snr_best`. Defaults to 5.
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
                           folder = ".", filename,
                           eval = FALSE,
                           tries = 2,
                           onFail = "continue",
                         snr_best = 15, snr_good = 5) {

  if (length(filename) == 0) stop("You must enter a filename for the recorded sound!")
  if (folder == "."){folder <- getwd()}
  shiny::moduleServer(
    id = id,

    function(input, output, session, mod_id = id) {
      ns <- session$ns
      feedback <- shiny::reactiveValues(try = 1,
                                        tips = NA,
                                        score = NA)
      shiny::observe({
        params <- list(start = ns("start"),
                       stop = ns("stop"),
                       output = paste0(mod_id, "-audio"))

        session$sendCustomMessage("recordAudio",
                                  params)
      })

      shiny::observeEvent(input$start, {
        shinyjs::disable("start")
        shinyjs::delay(500, shinyjs::enable("stop"))

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
        shinyjs::delay(2000, shinyjs::enable("start"))
        if (isTRUE(evalWav)) {
          wave <- tuneR::readWave(filename = file.path(folder, filename))
          eval <- evalWav(wave)
          feedback <- list()
          if (eval$snr >= snr_best) {
            snr_score <- 2
            snr_tip <- character(0)
          } else if (eval$snr >= snr_good) {
            snr_score <- 1
            snr_tip <- c("<li>Sounds pretty good! Double-check your environment to make sure it's as quiet as possible.</li>")
          } else {
            snr_score <- 0
            snr_tip <- c("<li>Your environment is a bit on the noisy side. Double-check to make sure you've eliminated as much background noise as possible.</li><li>Try moving a little closer to your microphone</li>")
          }

          if (eval$clipped == FALSE) {
            clip_score <- 2
            clip_tip <- character(0)
          } else {
            clip_score <- 0
            clip_tip <- c("<li>Whoa! That recording was a bit loud. Try moving back from your microphone a little.</li>")
          }

          if (feedback$try < tries) {
            if (sum(snr_score, clip_score) == 4){
              feedback$tips <- c("<h4>Sounds good!</h4>")
            } else {
              feedback$tips <- paste0("<h5>Tips:</h5><ul", snr_tip, clip_tip, "</ul>")
            }
            feedback$score <- snr_score + clip_score + 1
            if (feedback$score >= 3) {
              shinyalert::shinyalert(
                type = "success",
                title = "Success!",
                text = paste0(results$tips),
                html = TRUE,
                timer = 4000)
            } else {
              shinyalert::shinyalert(
                type = "warning",
                title = "Hmm...",
                text = paste0(results$tips, "<h5>Click the button below to try again and help us complete the calibration of our system to your environment.</h5>"),
                confirmButtonText = "Try Again",
                html = TRUE,
                timer = 4000)
            }
           feedback$try <- feedback$try + 1
          } else {
            if (onFail == "continue") {
              feedback$score <- 5
              feedback$tips <- c("<h6>Sounds good!</h6>")
              shinyalert::shinyalert(
                type = "success",
                title = "Success!",
                text = paste0(results$tips),
                html = TRUE,
                timer = 4000)
            } else {
              feedback$score <- snr_score + clip_score + 1
              feedback$tips <- c("<h6>Unfortunately, you do not seem to have the right equipment for this task. Thank you for your interest in the study!")
              shinyalert::shinyalert(
                type = "error",
                title = "Error:",
                text = paste0(results$tips),
                html = TRUE,
                timer = 4000)
            }
          }

        }
        })
    }
  )
}
