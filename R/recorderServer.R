#' Server function for Recording User Audio
#'
#' @description The server function for recording user audio enables the 'stop' button after the user begins recording and checks to make sure the user has given the website permission to record audio in their browser. Requires the UI \code{\link{recorderUI}}.
#' @param id The input ID associated with the recorder module. Must be the same as the id of `recorderUI()`.
#' @param folder Character. Where to store the audio file. Defaults to the path returned by `getwd()`
#' @param filename Required. Character. The name of the file to be recorded. Do not specify a directory here. Instead, do that with the `folder` argument.
#' @param type Defaults to "wav". The current version of `speechcollectr` only supports wav file recording.
#' @param numChannels Integer. Must be 1 or 2. Should the sample be recorded with 1 channel (mono) or 2 channels (stereo)? Defaults to 1 (ideal for most speech recordings).
#' @param sampleRate Integer. How many samples per second should be recorded in the audio file? Defaults to 44100 (ideal for most speech recordings). NOTE: In some cases, the default of the user's browser might override the value given here.
#'
#' @return Saves an audio file with the specified `filename` in a directory specified by `folder`.
#' @family Audio recording module
#' @seealso Must be used with \code{\link{recorderUI}}.

#' @export
#'
#' @examples
#' #' if (interactive()) {
#' ui <- shiny::fluidPage(
#'   recorderUI("record")
#' )
#' server <- function(input, output, session) {
#'   recorderServer(id = "record", filename = "sample.wav")
#' }
#' shiny::shinyApp(ui = ui, server = server)
#' }
#'
recorderServer <- function(id = "recorder",
                           folder = ".", filename,
                           type = "wav", numChannels = 1,
                           sampleRate = 44100) {

  if (length(filename) == 0) stop("You must enter a filename for the recorded sound!")
  if (folder == "."){folder <- getwd()}
  shiny::moduleServer(
    id = id,

    function(input, output, session) {
      shiny::observe(
        shinyjs::js$webAudioRecorder(type = type,
                                     numChannels = numChannels,
                                     sampleRate = sampleRate)
      )

      shiny::observeEvent(input$start, {
        shiny::observeEvent(input$ready, {
          shinyjs::disable("start")
          shinyjs::delay(500, shinyjs::enable("stop"))
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
        shinyjs::delay(2000, shinyjs::enable("start")) })
    }
  )
}
