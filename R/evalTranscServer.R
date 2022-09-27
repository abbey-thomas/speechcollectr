#' Evaluate Transcription and Give Feedback in a Shiny App
#'
#' @param filename An RDS file output from `transcribeServer()` OR an RDS file containing a single character string representing the entered transcription.
#' @param text A character string to test against the correct transcription. Not necessary if `filename` is supplied.
#' @param correct A character string of the correct transcription to which `text` or `filename` will be compared.
#' @param alphaOnly Boolean. Should the transcription be compared only in alphabetic characters entered, ignoring any spaces, punctuation, or digits? Defaults to TRUE.
#' @param matchCase Boolean. Should the comparison be case-sensitive? Defaults to FALSE.
#' @param attempts Integer. How many attempts does the user get to enter a correct transcription?
#' @param counter Integer. Usually a shiny reactive value. Number of attempts the participant has completed.
#' @param rmIncorrect Boolean. Should files containing incorrect transcriptions be kept if the participant has attempts remaining? Defaults to TRUE.
#'
#' @note The parameter `rmIncorrect` removes incorrect transcriptions BEFORE the participants' final attempt. This means that the final transcription the participant enters will never be deleted. This parameter was added because, without it, the program will evaluate the current attempt and all previous attempts that have been saved under a given filename, returning numerous feedback screens any time more than one attempt is completed.
#' @return A Boolean value indicating whether the transcription in `filename` matches the value given in `correct` (=1) or not (=0). This function will also return feedback to the user in dialog boxes, indicating success (if a correct transcription was entered); a warning (upon entry of an incorrect transcription, informing participants of how many attempts remain); or an error alert (indicating an incorrect transcription when all attempts have been used).
#' @export
#'
#' @details Though transcriptions can be evaluated the basic transcribeServer() function, this function allows users more flexibility by letting them choose when they want to check the transcription and whether it should be checked for case and non-alphanumeric characters.
#'
#' @examples
#' data("samp_wav")
#' tuneR::writeWave(samp_wav, "sample.wav")
#' wwwPrep(from = "sample.wav")
#'
#' if (interactive()) {
#'  library(shiny)
#'  library(speechcollectr)
#'  library(shinyjs)
#
#'  ui <- fluidPage(
#'   actionButton("begin", "Begin"),
#'   transcribeUI(id = "transcribe"),
#'   h5("Click 'Begin' each time you want to transcribe. The recording will not change from trial to trial."),
#'   h5("To demonstrate the transcription evaluation procedure, we'll use a different 'correct transcritiption' for each trial."),
#'   h5("The correct answers for the first 4 trials are: 'a', 'ab', 'abc', 'abcd' (in that order).")
#'  )
#'
#'
#'  server <- function(input, output, session) {
#'   rvs <- reactiveValues(n = 1, attempt = 1)
#'   answers <- c("a", "ab", "abc", "abcd")
#'
#'   observeEvent(input$begin, {
#'     disable("begin")
#'     transcribeServer(id="transcribe",
#'                      audioFile = "sample.wav",
#'                      n_play = 4,
#'                      outFile = paste0("sample", rvs$n, ".rds"),
#'                      result = "hide")
#'   })
#'
#'   observeEvent(input[["transcribe-submit"]], {
#'     enable("begin")
#'
#'     rvs$is_correct <- evalTranscServer(filename = paste0("sample", rvs$n, ".rds"),
#'                                        counter = rvs$attempt,
#'                                        attempts = 2,
#'                                        matchCase = TRUE,
#'                                        correct = answers[rvs$n])
#'
#'     shiny::req(!is.null(rvs$is_correct))
#'     if (rvs$is_correct == 1) {
#'       rvs$n <- rvs$n + 1
#'     } else {
#'       rvs$attempt <- rvs$attempt + 1
#'     }
#'   })
#'
#'  }
#'  shinyApp(ui = ui, server = server)
#' }

evalTranscServer <- function(filename = NULL,
                             text,
                             correct,
                             alphaOnly = TRUE,
                             matchCase = FALSE,
                             attempts = NULL,
                             counter = 1,
                             rmIncorrect = TRUE) {

  if (!is.null(filename)) {
    shiny::req(file.exists(filename))
    entered <- readRDS(filename)

    if ("entered" %in% names(entered)) {
      text <- entered["entered"]
    }
  } else {
    text <- text
  }


  is_correct <- evalTransc(text = text,
                           correct = correct,
                           alphaOnly = alphaOnly,
                           matchCase = matchCase)
  if (is_correct == 1) {
    shinyalert::shinyalert(title = "That's correct!",
                           type = "success",
                           session = shiny::getDefaultReactiveDomain())
  } else {

    if (is.null(attempts)) {
      shinyalert::shinyalert(title = "Please try again.",
                             text = "Your transcription contains at least one error.",
                             type = "error",
                             session = shiny::getDefaultReactiveDomain())

    } else {
      if (counter <= attempts) {
        shinyalert::shinyalert(title = "Please try again.",
                               text = paste0("Your transcription contains at least one error. You have ",
                                             attempts-counter, " attempt(s) remaining."),
                               type = "warning",
                               session = shiny::getDefaultReactiveDomain())

        if (isTRUE(rmIncorrect)) {
          file.remove(filename)
        }
      } else {
        shinyalert::shinyalert(title = "That's incorrect.",
                               text="You have used all available attempts for transcribing this audio.",
                               confirmButtonText = "Close",
                               type = "error",
                               session = shiny::getDefaultReactiveDomain())
      }
    }
  }
  return(is_correct)
}
