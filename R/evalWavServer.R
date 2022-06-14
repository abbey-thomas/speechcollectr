#' Give feedback to participants on recording quality
#'
#' @param wave Required. Either a `tuneR::Wave` object or a valid file path to an existing wav file.
#' @param counter Required. A reactive value that tells this function how many tries a participant has had so far.
#' @param min_sf What is the minimum sample rate (in Hz) that you will allow for the recording? If a user's browser will not allow audio recording at this high of a sample rate, the user will get an error message. Set to 0 if you do not want to exclude participants who record at low sampling rates.
#' @param snr_best Integer. If eval=TRUE, what is the minimum SNR (dB) required for the recording to be considered of the best quality? Defaults to 15.
#' @param snr_good Integer. If eval=TRUE, what is the minimum SNR (dB) required for the recording to be considered acceptable? Must be less than `snr_best`. Defaults to 5.
#' @param max_clip Numeric. What proportion of the frames can be clipped and the recording still be acceptable? Defaults to .01.
#' @param tries Integer. How many tries should the user get to create a quality recording?
#' @param onFail If eval=TRUE, what kind of message should the user receive if they try to record the maximum number of `tries` and the quality is still poor? Must be either "stop" (user gets an error message) or "continue" (user gets a success message and we ignore recording quality).
#'
#' @return A boolean value indicating success (=1) or failure (=0) to produce a quality recording.
#' @export
#'
#' @examples
#' if (interactive()) {
#' ui <- shiny::fluidPage(
#'   shiny::actionButton("go", "GO"),
#'   recordUI(id = "record"),
#'   shiny::actionButton("submit", "SUBMIT")
#' )
#'
#' server <- function(input, output, session) {
#'  rvs <- shiny::reactiveValues(n=0)
#'
#'  shiny::observeEvent(input$go, {
#'    rvs$rec <- recordServer(id = "record",
#'                          trigger = shiny::reactive(input$go),
#'                          filename = "sample.wav")
#'  })
#'
#'  shiny::observeEvent(input$submit, {
#'    shiny::req(rvs$rec)
#'    rvs$result <- evalWavServer(wave = "sample.wav",
#'                                trigger = shiny::reactive(input$submit),
#'                                counter = rvs$n)
#'  })
#'
#'  shiny::observe({
#'    shiny::req(rvs$result)
#'    shinyjs::delay(500, {rvs$rec <- NULL})
#'  })
#' }
#' shiny::shinyApp(ui = ui, server = server)
#' }
#'
evalWavServer <- function(wave,
                          counter,
                          min_sf = 44100,
                          snr_best = 15, snr_good = 5,
                          max_clip = 0.01,
                          tries = 3,
                          onFail = "continue"){
  if (class(wave)[1] != "Wave") {
    wave <- tuneR::readWave(wave)
  }

  eval <- evalWav(wave)

  if (eval$samp.rate < min_sf) {
    shinyalert::shinyalert(
      type = "error",
      title = "Error:",
      text = "Your browser does not support high-quality audio recording. Please try a different device or browser.",
      confirmButtonText = "Exit Experiment")
    feedback <- 0
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

    if (counter < tries) {
      if (sum(snr_score, clip_score) == 4){
        tips <- c("<h4>Sounds good!</h4>")
      } else {
        tips <- paste0("<h5>Tips:</h5><ul", snr_tip, clip_tip, "</ul>")
      }
      score <- snr_score + clip_score

      if (score >= 3) {
        shinyalert::shinyalert(
          type = "success",
          title = "Success!",
          text = paste0(tips),
          confirmButtonText = "Confirm Submission",
          html = TRUE)
        feedback <- 1

      } else {
        shinyalert::shinyalert(
          type = "warning",
          title = "Hmm...",
          text = paste0(tips, "<h5>Click the button below to try again and help us complete the calibration of our system to your environment.</h5>"),
          confirmButtonText = "Try Again",
          html = TRUE,
          inputId = ns("alert"),
          closeOnEsc = FALSE)
        counter <- counter + 1
        feedback <- 0
      }
    } else {
      if (onFail == "continue") {
        tips <- c("<h6>Sounds good!</h6>")
        shinyalert::shinyalert(
          type = "success",
          title = "Success!",
          text = paste0(tips),
          confirmButtonText = "Confirm Submission",
          html = TRUE)
        feedback <- 1

      } else {
        tips <- c("<h6>Unfortunately, you do not seem to have the right equipment for this task. Thank you for your interest in the study! You may close this browser window.")
        shinyalert::shinyalert(
          type = "error",
          title = "Error:",
          text = paste0(tips),
          confirmButtonText = "Exit Experiment",
          html = TRUE)
        feedback <- 0

      }
    }
  }
  return(feedback)
}
