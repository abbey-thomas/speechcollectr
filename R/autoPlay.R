#' Play Audio without an Explicit Play Button
#'
#' @param audioId character. The id of the audio element.
#' @param src character. The name of a WAV file in the www folder.
#'
#' @return When included in a reactive observer (`observe()` or `observeEvent()`), plays an audio file without a click from a participant.
#' @export
#'
#' @examples
#' # NOTE: This example will only play audio from a saved app script in the same directory as a 'www' subdirectory that contains the audio file.
#' data("samp_wav")
#' tuneR::writeWave("samp_wav", "www/sample.wav")
#'
#' library(shiny)
#'
#' ui <- tabsetPanel(id = "tabs",
#'                   tabPanel(
#'                     "tab1",
#'                     "The sound is not playing yet. Click on tab2 to play the sound."
#'                   ),
#'                   tabPanel(
#'                     "tab2",
#'                     "The sound should be playing now."
#'                   )
#' )
#'
#' server <- function(input, output, session) {
#'   observeEvent(input$tabs, {
#'     if (input$tabs == "tab2") {
#'       autoPlay(audioId = "sample", src = "sample.wav")
#'     }
#'   })
#' }
#'
autoPlay <- function(audioId, src){

  if (grepl("^www/", src)) {
    src <- sub("^www/", "", src)
  }
  session <- shiny::getDefaultReactiveDomain()

  shiny::insertUI(selector = "head",
                  where = "afterEnd",
                  ui = htmltools::tags$audio(
                    id = audioId,
                    src = src,
                    type = "audio/wav",
                    autoplay = NA, controls = NA,
                    style = "display:none;"),
                  session = session
  )
}
