#' Give feedback to participants on recording quality
#'
#' @param wave Required. Either a `tuneR::Wave` object or a valid file path to an existing wav file.
#' @param counter Required. A value that tells this function how many tries a participant has had so far.
#' @param min_sf What is the minimum sample rate (in Hz) that you will allow for the recording? If a user's browser will not allow audio recording at this high of a sample rate, the user will get an error message. Set to 0 if you do not want to exclude participants who record at low sampling rates.
#' @param snr_best Integer. If eval=TRUE, what is the minimum SNR (dB) required for the recording to be considered of the best quality? Defaults to 30.
#' @param snr_good Integer. If eval=TRUE, what is the minimum SNR (dB) required for the recording to be considered acceptable? Must be less than `snr_best`. Defaults to 20.
#' @param max_clip Numeric. What proportion of the frames can be clipped and the recording still be acceptable? Defaults to .01.
#' @param tries Integer. How many tries should the user get to create a quality recording?
#' @param onFail What kind of message should the user receive if they try to record the maximum number of `tries` and the quality is still poor? Must be either "stop" (the default, user gets an error message) or "continue" (user gets a success message and we ignore recording quality).
#'
#' @return One of three reactive values: "pass"=recording of sufficient quality; "warn"=recording of insufficient quality, but the participant has had fewer attempts than the number of `tries`; or "fail"=the recording is of insufficient quality and the participant has used all available attempts.
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
#'     tags$head(
#'       useRecorder(directory = "www")
#'     ),
#'
#'     actionButton("next_trial", "NEXT"),
#'
#'     ## Setup page layout
#'     hidden(div(id = "trialDiv",
#'                style = "text-align:center;",
#'
#'                hidden(div(id = "textDiv",
#'                           h4("Please record yourself reading this sentence aloud:"),
#'                           h2(textOutput(outputId = "read_this")))),
#'
#'                ## Create the buttons for controlling the recording
#'                actionButton(inputId = "start",
#'                             label = "start"),
#'
#'                ### Hide the stop button until user clicks start
#'                hidden(actionButton(inputId = "stop",
#'                                    label = "stop"))
#'     ))
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
#'       showElement("trialDiv")
#'
#'       ### Hide the next button
#'       hide("next_trial")
#'     })
#'
#'     ## When the start button is clicked
#'     observeEvent(input$start, {
#'
#'       ### Start the recording
#'       startRec()
#'
#'       ### Disable the start button
#'       disable("start")
#'
#'       ### Show the stop button
#'       delay(500, showElement("stop"))
#'     })
#'
#'     ## When the user gives permission to record....
#'     observeEvent(input[["rec-ready"]], {
#'
#'       ### Show the text they should read
#'       showElement("textDiv")
#'       output$read_this <- renderText({paste0("This is recording ",
#'                                              rvs$trial_n, ".")})
#'     })
#'
#'
#'     ## When the user clicks stop
#'     observeEvent(input$stop, {
#'
#'       ### Stop recording
#'       stopRec(filename = paste0("rec", rvs$trial_n, ".wav"))
#'
#'       ### Enable the start button
#'       enable("start")
#'
#'       ### Hide the stop button
#'       hide("stop")
#'       hide("textDiv")
#'     })
#'
#'     ## Once the wav file exists...
#'     observeEvent(input[["rec-done"]], {
#'       ### Evaluate the recording
#'       evalWavServer(wave = paste(input[["rec-done"]]))
#'     })
#'
#'     ## Once the wav file has been evaluated...
#'     observeEvent(input[["evalWav-result"]], {
#'
#'       ### If the recording is of sufficient quality...
#'       if (input[["evalWav-result"]] == "pass") {
#'
#'         #### Hide the recording interface
#'         hide("trialDiv")
#'
#'         #### And show the "next" button so that the participant can record the next file
#'         showElement("next_trial")
#'       }
#'     })
#'   }
#'
#'   # Run the application
#'   shinyApp(ui = ui, server = server)
#' }
#'
evalWavServer <- function(wave,
                          counter = 1,
                          min_sf = 44100,
                          snr_best = 30,
                          snr_good = 20,
                          max_clip = 0.01,
                          tries = Inf,
                          onFail = "stop"){
  if (class(wave)[1] != "Wave") {
    wave <- suppressWarnings(tuneR::readWave(wave))
  }

  session <- shiny::getDefaultReactiveDomain()
  eval_rv <- shiny::reactiveValues(result = NULL)

  eval <- evalWav(wave)

  if (shiny::is.reactive(counter)) { counter <- shiny::isolate(counter)}

  if (eval$samp.rate < min_sf) {
    eval_rv$result <- "fail"

    shinyalert::shinyalert(type = "error", title = "Error:",
                           text = "Your browser does not support high-quality audio recording. Please try a different device or browser.",
                           confirmButtonText = "Exit Experiment",
                           inputId = "evalWav-fail",
                           closeOnEsc = FALSE)
  } else {
    if (is.na(eval$snr)) {
      snr_tip <- c("<li>Your recording was too quiet to be analyzed. Try getting closer to your microphone.</li>")
      snr_score <- 0
    }
    else if (eval$snr >= snr_best) {
      snr_tip <- character(0)
      snr_score <- 2
    }
    else if (eval$snr >= snr_good) {
      snr_score <- 1
      snr_tip <- c("<li>Sounds good! Double-check your environment to make sure it's as quiet as possible.</li>")
    }
    else {
      snr_score <- 0
      snr_tip <- c("<li>Your environment is quite noisy. Double-check to make sure you've eliminated as much background noise as possible.</li><li>Try moving a little closer to your microphone</li>")
    }
    if (is.na(eval$clipped)) {
      clip_tip <- character(0)
      clip_score <- 0
    }
    else if (eval$clipped <= max_clip) {
      clip_tip <- character(0)
      clip_score <- 2
    }
    else {
      clip_score <- 0
      clip_tip <- c("<li>Whoa! That recording was loud. Try moving back from your microphone a little.</li>")
    }
    if (counter <= tries) {
      if (sum(snr_score, clip_score) == 4) {
        tips <- c("<h4>Sounds good!</h4>")
      }
      else {
        tips <- paste0("<h5>Tips:</h5><ul", snr_tip,
                       clip_tip, "</ul>")
      }
      score <- snr_score + clip_score
      if (score >= 3) {
        eval_rv$result <- "pass"

        shinyalert::shinyalert(type = "success", title = "Success!",
                               text = paste0(tips),
                               confirmButtonText = "Confirm Submission",
                               html = TRUE,
                               closeOnEsc = FALSE)
      } else {
        if (counter < tries) {
          eval_rv$result <- "warn"

          shinyalert::shinyalert(type = "warning", title = "Hmm...",
                                 text = paste0(tips, "<h5>Click the button below to try again and help us complete the calibration of our system to your environment.</h5>"),
                                 confirmButtonText = "Try Again",
                                 html = TRUE,
                                 closeOnEsc = FALSE)
          counter <- counter + 1
        } else {
          if (onFail == "continue") {
            eval_rv$result <- "pass"

            tips <- c("<h6>Sounds good!</h6>")
            shinyalert::shinyalert(type = "success", title = "Success!",
                                   text = paste0(tips),
                                   confirmButtonText = "Confirm Submission",
                                   html = TRUE,
                                   closeOnEsc = FALSE)
          }
          else {
            eval_rv$result <- "fail"

            tips <- c("<h6>Unfortunately, you do not seem to have the right equipment for this task. Thank you for your interest in the study! You may close this browser window.")
            shinyalert::shinyalert(type = "error", title = "Error:",
                                   text = paste0(tips),
                                   confirmButtonText = "Exit Experiment",
                                   html = TRUE,
                                   closeOnEsc = FALSE)
          }
        }
      }
    }

  }

  shiny::observeEvent(eval_rv$result, {
    if (!is.null(eval_rv$result)) {
      result <- paste(eval_rv$result)
      session$sendCustomMessage("evalWavResult", result)
    }
  })
}
