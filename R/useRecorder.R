#' Use the recording scripts to record audio
#'
#' @return Adds the javascript files for audio recording to the ui object.
#' @export
#'
#' @examples
useRecorder <- function() {
  ui <- shiny::tagList(
    shiny::includeScript(system.file("recorder/rec_backend.js",
                                     package = "speechcollectr")),
    shiny::includeScript(system.file("recorder/rec_frontend.js",
                                     package = "speechcollectr"))
  )

  return(ui)
}
