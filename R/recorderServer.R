#' Server function for Recording User Audio
#'
#' @description The server function for recording user audio enables the 'stop' button after the user begins recording and checks to make sure the user has given the website permission to record audio in their browser. Requires the UI \code{\link{recorderUI}}.
#' @param id The input ID associated with the recorder module. Must be the same as the id of `recorderUI()`.
#' @param folder Character. Where to store the audio file. Defaults to the path returned by `getwd()`
#' @param filename Required. Character. The name of the file to be recorded. Do not specify a directory here. Instead, do that with the `folder` argument.
#'
#' @return Saves a single-channel, 16-bit, 44.1 kHz sample rate WAV file with the specified `filename` in a directory specified by `folder`.
#' @family Audio recording module
#' @seealso Must be used with \code{\link{recorderUI}}.
#' @export
#' @examples
#' if (interactive()) {
#' ui <- shiny::fluidPage(
#'   recorderUI(id = "record")
#' )
#'
#' server <- function(input, output, session) {
#'  recorderServer(id = "record", filename = "sample.wav")
#' }
#' shiny::shinyApp(ui = ui, server = server)
#' }
#'
# At the end of this process which button should be enabled?
# How can users add other functions to the buttons in the module?
recorderServer <- function(id = "recorder",
                           folder = ".", filename) {

  if (length(filename) == 0) stop("You must enter a filename for the recorded sound!")
  if (folder == "."){folder <- getwd()}
  shiny::moduleServer(
    id = id,

    function(input, output, session, mod_id = id) {
      ns <- session$ns
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
        shinyjs::delay(2000, shinyjs::enable("start")) })
    }
  )
}
