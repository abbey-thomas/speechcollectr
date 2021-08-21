#' Load Audio and Create a Play Button to Start Playback
#'
#' @param inputId The input ID of the play button.
#' @param src The filename for the audio file to play. Must be in the "www/" folder for the app.
#' @param audioId The input ID to associate with the audio file that will be loaded.
#' @param label An optional text label to add to the button. Defaults to blank. NOTE: If icon is `NULL`, this variable should be given a value explicitly.
#' @param inline Boolean. If FALSE (the default), the oause button will be on its own line in the UI (i.e., wrapped in a `div` tag). If TRUE, the button will be on a line with the preceeding shiny UI object (i.e., wrapped in a `span` tag).
#' @param icon An optional `shiny::icon()` to add to the button. Defaults to the font-awesome "play" icon. NOTE: If no text label is present, icon cannot be null.
#'
#' @return An audio file with the ID `audioId` will be added to the list of input values. On the server side of a shiny app, this function returns an integer of class "shinyActionButtonValue", which means it can be used in all the ways an ordinary shiny action button can (e.g., you can add functions to be called when the button is pressed with `shiny::observeEvent()` and `shiny::eventReactive`).
#' @export
#'
#' @examples
#' # First use www_create() to get a wav file in the correct directory.
#' www_create(volumeCalibration = TRUE)
#'
#' ## Only run examples in interactive R sessions
#' if (interactive) {
#' ui <- fluidPage(
#'   "Play a sound: "
#'    playBttn(inputId = "play", src = "cal_noise.wav",
#'         audioId = "cal_noise")
#'    disabled(pauseBttn(inputId = "pause", audioId = "cal_noise"))
#'  )
#'
#' # disable the play button and enable the pause button after play button is clicked.
#' server <- function(input, output, session) {
#'  observeEvent(input$play) {
#'    disable("play")
#'    enable("pause")
#'  }
#' }
#' shinyApp(ui = ui, server = server)
#' }
#'
#' ## Use inline = TRUE to display the button on the same line as the preceding UI element.
#' if (interactive) {
#' ui <- fluidPage(
#'   "Play a sound: "
#'    playBttn(inputId = "play", src = "cal_noise.wav",
#'         audioId = "cal_noise", inline = TRUE)
#'    pauseBttn(inputId = "pause", audioId = "cal_noise", inline = TRUE)
#'  )
#'
#' server <- function(input, output, session) {}
#'
#' shinyApp(ui = ui, server = server)
#' }
#'
#' ## Use parameter 'label =' to add a text label to the button.
#' ## Use icon = NULL to remove the icon.
#' if (interactive) {
#' ui <- fluidPage(
#'    playBttn(inputId = "play", label = "Play noise", src = "cal_noise.wav",
#'         audioId = "cal_noise", inline = TRUE, icon = NULL)
#'    pauseBttn(inputId = "pause", audioId = "cal_noise", inline = TRUE)
#'  )
#'
#' server <- function(input, output, session) {}
#'
#' shinyApp(ui = ui, server = server)
#' }
playBttn <- function(inputId, src, audioId, label = "",
                     inline = FALSE, icon = "play") {
  jsCode <- htmltools::HTML(
    "playAudio = function(audioId) {
      document.getElementById(audioId).play();
    }"
  )

  ui <- htmltools::tags$span(
    htmltools::tags$head(
      htmltools::tags$audio(id = audioId,
                            src = src,
                            type = "audio/wav",
                            style="display:none;"),
      htmltools::tags$script(jsCode)),
    shiny::actionButton(inputId = inputId,
                        label = label,
                        icon = shiny::icon(icon),
                        inline = inline,
                        onclick = paste0("playAudio('", audioId, "')")
    )
  )
  return(ui)
}
