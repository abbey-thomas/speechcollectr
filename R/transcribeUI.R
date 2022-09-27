#' Build a user interface for transcribing audio files
#'
#' @param id The module id. Must be the same as \code{\link{transcribeServer}}.
#' @param align One of "left", "center" or "right". Default is "center". Describes the alignment of all elements in the module.
#'
#' @return A shiny user interface for playing audio and entering text transcription.
#' @export
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

transcribeUI <- function(id = "transcribe",
                         align = "center"){
  ui <- shiny::tagList(
    shinyjs::useShinyjs(),
    shinyjs::hidden(shiny::tags$div(id = paste0(id),
                                    style = paste0("text-align:", align, ";"),
                                    shiny::uiOutput(paste0(id, "-ui"))
    ))
  )

  return(ui)
}
