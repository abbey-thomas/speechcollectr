#' Create UI to Record User Audio
#'
#' @description The user-interface side of the Shiny module to record user audio in Speech Production experiments. A participant chooses when to begin recording and when to stop by clicking the buttons created in this module. Requires the server-side function \code{\link{recorderServer}}.
#' @param id The input ID associated with the recorder module. Must be the same as the id of `recorderServer()`.
#' @param startText Character. The text label for the "start recording" button. Defaults to "RECORD".
#' @param startTextCol A valid color name in R or a hexidecimal color code denoting the color of the start button text. Defaults to "white".
#' @param startFillCol A valid color name in R or a hexidecimal color code denoting the color of the start button text. Defaults to the "green" hex code from Paul Tol's colorblind-safe \emph{bright} qualitative color scheme.
#' @param stopText Character. The text label for the "stop recording" button. Defaults to "STOP".
#' @param stopTextCol A valid color name in R or a hexidecimal color code denoting the color of the start button text. Defaults to "black".
#' @param stopFillCol A valid color name in R or a hexidecimal color code denoting the color of the start button text. Defaults to Defaults to the "red" hex code from Paul Tol's colorblind-safe \emph{bright} qualitative color scheme.
#' @param startInline Boolean. Should the 'start recording' button be on the same line as the preceding UI element?
#' @param stopInline Boolean. Should the 'stop recording' button be placed on the same line as the 'start recording' button?
#'
#' @return A user interface containing buttons to control recording of participant audio.
#' @export
#' @family Audio recording module
#' @seealso Must be used with \code{\link{recorderServer}}. Paul Tol's colorblind-safe palettes (the source of the default button colors) can be found at \url{https://personal.sron.nl/~pault/#sec:qualitative}.
#' @examples
#' if (interactive()) {
#' ui <- shiny::fluidPage(
#'   recorderUI("record")
#' )
#' server <- function(input, output, session) {
#'   recorderServer(id = "record", filename = "sample.wav")
#' }
#' shiny::shinyApp(ui = ui, server = server)
#' }
#'
#' @importFrom grDevices col2rgb
#'
recorderUI <- function(id = "recorder",
                       startText = "RECORD",
                       startTextCol = "white",
                       startFillCol = "#228833",
                       stopText = "STOP",
                       stopTextCol = "black",
                       stopFillCol = "#EE6677",
                       startInline = FALSE,
                       stopInline = TRUE) {
  ns <- shiny::NS(id)

  if (tryCatch(is.matrix(col2rgb(startFillCol)),
               error = function(e) FALSE) == FALSE) {
    stop("Error: startFillCol argument requires a valid color name or hexadecimal code.")
  }

  if (tryCatch(is.matrix(col2rgb(startTextCol)),
               error = function(e) FALSE) == FALSE) {
    stop("Error: startTextCol argument requires a valid color name or hexadecimal code.")
  }

  if (tryCatch(is.matrix(col2rgb(stopFillCol)),
               error = function(e) FALSE) == FALSE) {
    stop("Error: stopFillCol argument requires a valid color name or hexadecimal code.")
  }

  if (tryCatch(is.matrix(col2rgb(stopTextCol)),
               error = function(e) FALSE) == FALSE) {
    stop("Error: stopTextCol argument requires a valid color name or hexadecimal code.")
  }

  shiny::addResourcePath("js", system.file("js", package = "speechcollectr"))
  shiny::addResourcePath("WAR", system.file("js", "WAR", package = "speechcollectr"))

  ui <- shiny::tagList(
    shinyjs::useShinyjs(),
    shiny::includeScript(file.path("WAR", "WebAudioRecorder.min.js")),
    shinyjs::extendShinyjs(
      script = file.path("js", "extendjs_recorder.js"),
      functions = c("WebAudioRecorder"),

      shiny::actionButton(ns("start"), label = startText,
                          style = paste0("color: ", col2rgb(startTextCol),
                                         "; background-color: ",
                                         col2rgb(startFillCol)),
                          inline = startInline),

      shinyjs::disabled(
        shiny::actionButton(ns("stop"), label = stopText,
                            style = paste0("color: ", col2rgb(stopTextCol),
                                           "; background-color: ",
                                           col2rgb(stopFillCol)),
                            inline = stopInline))

    )
  )
}
