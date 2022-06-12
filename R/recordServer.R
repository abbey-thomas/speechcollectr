#' Server function for Recording User Audio
#'
#' @description The server function for recording user audio enables the 'stop' button after the user begins recording and checks to make sure the user has given the website permission to record audio in their browser. Requires the UI \code{\link{recordUI}}.
#' @param id The input ID associated with the record module. Must be the same as the id of `recordUI()`.
#' @param trigger A reactive value indicating the event that should trigger the appearance of the consent form. May be an `input$...` value from outside the module wrapped in `reactive()`.
#' @param folder Character. Where to store the audio file. Defaults to the path returned by `getwd()`
#' @param filename Required. Character. The name of the file to be recorded. Do not specify a directory here. Instead, do that with the `folder` argument.
#' @param writtenStim A character vector that you want a participant to read while recording.
#' @param writtenDelay Integer. How many milliseconds should elapse between the time the participant clicks `record` and the time the written stimulus appears? Defaults to 500. We recommend not using a value less than that.
#' @param submitButtonText Character. Text label that will appear on the button the participant will use to submit the recording.
#' @param playback Boolean. Should the participant be allowed to listen to the recording before submitting? Defaults to `FALSE`.
#' @param eval Boolean. Should we evaluate the recording for sample rate, clipping, and SNR, and give the user tips on how to improve if the recording quality is poor? Defaults to FALSE.
#' @param tries Integer. How many tries should the user get to record?
#' @param onFail If eval=TRUE, what kind of message should the user receive if they try to record the maximum number of `tries` and the quality is still poor? Must be either "stop" (user gets an error message) or "continue" (user gets a success message and we ignore recording quality).
#' @param min_sf What is the minimum sample rate (in Hz) that you will allow for the recording? If a user's browser will not allow audio recording at this high of a sample rate, the user will get an error message. Set to 0 if you do not want to exclude participants who record at low sampling rates.
#' @param snr_best Integer. If eval=TRUE, what is the minimum SNR (dB) required for the recording to be considered of the best quality? Defaults to 15.
#' @param snr_good Integer. If eval=TRUE, what is the minimum SNR (dB) required for the recording to be considered acceptable? Must be less than `snr_best`. Defaults to 5.
#' @param max_clip Numeric. What proportion of the frames can be clipped and the recording still be acceptable? Defaults to .01.
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
                         submitButtonText = "SUBMIT RECORDING",
                         playback = FALSE,
                         eval = FALSE,
                         tries = 2,
                         onFail = "continue",
                         min_sf = 44100,
                         snr_best = 15, snr_good = 5,
                         max_clip = 0.01) {

  if (length(filename) == 0) stop("You must enter a filename for the recorded sound!")
  if (folder == "."){folder <- getwd()}
  shiny::moduleServer(
    id = id,

    function(input, output, session, mod_id = id) {
      ns <- session$ns
      feedback <- shiny::reactiveValues(try = 0,
                                        tips = NA,
                                        score = 0,
                                        eval_try = 1,
                                        result = 0)
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

        if (shiny::isTruthy(input$ready)) {
          shinyjs::delay(500, shinyjs::enable("stop"))
          if (!is.null(writtenStim)) {
            output$stim <- shiny::renderText(as.character(writtenStim))
            shinyjs::delay(as.numeric(writtenDelay),
                           shinyjs::showElement("stim_div"))
          }
        }

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
        feedback$try <- feedback$try + 1
        shinyjs::showElement("submission")

        output$submission <- shiny::renderUI({
          if (isTRUE(playback)) {
            playBttn(inputId = ns("replay"),
                     label = "Listen to your recording.",
                     src = file.path(folder, filename),
                     audioId = "recording",
                     fill = "white", text = "black")
          }
          shiny::tags$br()
          shiny::actionButton(ns("submit"), label = submitButtonText)
        })

        if (feedback$try < tries) {
          shinyjs::delay(500, shinyjs::enable("start"))
        } else {
          shinyalert::shinyalert(type = "info",
            text = "You have used all available attempts for this recording. Submit your most recent recording by clicking 'SUBMIT RECORDING' below.",
            confirmButtonText = "SUBMIT RECORDING",
            inputId = ns("submit2"),
            closeOnEsc = FALSE
          )
        }
        })

      shiny::observeEvent(input$submit|input$submit2, {
        shinyjs::hide("submission")
        shinyjs::hide("stim_div")
        shinyjs::hide("rec")
        feedback$result <- 1

        if (isTRUE(eval)) {
          wave <- tuneR::readWave(filename = file.path(folder, filename))
          eval <- evalWav(wave)

          if (eval$samp.rate < min_sf) {
            shinyalert::shinyalert(
              type = "error",
              title = "Error:",
              text = "Your browser does not support high-quality audio recording. Please try a different device or browser.",
              confirmButtonText = "Exit Experiment")
            feedback$result <- 0
          } else {
            if (eval$snr >= snr_best) {
              snr_tip <- character(0)
              snr_score <- 2
            } else if (eval$snr >= snr_good) {
              snr_score <- 1
              snr_tip <- c("<li>Sounds good! Double-check your environment to make sure it's as quiet as possible.</li>")
            } else {
              snr_score <- 0
              snr_tip <- c("<li>Your environment is quite noisy. Double-check to make sure you've eliminated as much background noise as possible.</li><li>Try moving a little closer to your microphone</li>")
            }

            if (eval$clipped <= max_clip) {
              clip_tip <- character(0)
              clip_score <- 2
            } else {
              clip_score <- 0
              clip_tip <- c("<li>Whoa! That recording was loud. Try moving back from your microphone a little.</li>")
            }

            if (feedback$eval_try < 3) {
              if (sum(snr_score, clip_score) == 4){
                feedback$tips <- c("<h4>Sounds good!</h4>")
              } else {
                feedback$tips <- paste0("<h5>Tips:</h5><ul", snr_tip, clip_tip, "</ul>")
              }
              feedback$score <- snr_score + clip_score

              if (feedback$score >= 3) {
                shinyalert::shinyalert(
                  type = "success",
                  title = "Success!",
                  text = paste0(feedback$tips),
                  confirmButtonText = "Confirm Submission",
                  html = TRUE)
              } else {
                shinyalert::shinyalert(
                  type = "warning",
                  title = "Hmm...",
                  text = paste0(feedback$tips, "<h5>Click the button below to try again and help us complete the calibration of our system to your environment.</h5>"),
                  confirmButtonText = "Try Again",
                  html = TRUE,
                  inputId = ns("alert"),
                  closeOnEsc = FALSE)
                feedback$eval_try <- feedback$eval_try + 1
                feedback$try <- 0
                feedback$result <- 0
              }
            } else {
              if (onFail == "continue") {
                feedback$tips <- c("<h6>Sounds good!</h6>")
                shinyalert::shinyalert(
                  type = "success",
                  title = "Success!",
                  text = paste0(feedback$tips),
                  confirmButtonText = "Confirm Submission",
                  html = TRUE)
              } else {
                feedback$tips <- c("<h6>Unfortunately, you do not seem to have the right equipment for this task. Thank you for your interest in the study! You may close this browser window.")
                shinyalert::shinyalert(
                  type = "error",
                  title = "Error:",
                  text = paste0(feedback$tips),
                  confirmButtonText = "Exit Experiment",
                  html = TRUE)
                feedback$result <- 0
              }
            }
          }
        }
      })

      shiny::observeEvent(input$alert, {
        shinyjs::showElement("stim_div")
        shinyjs::showElement("rec")
      })

      return(shiny::reactive(feedback$result))
    }
  )
}
