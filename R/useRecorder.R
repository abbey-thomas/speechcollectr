#' Use the recording scripts to record audio
#'
#' @param frontendJS Path to the frontend recording script. Downloaded with wwwPrep as "www/rec_frontend.js"
#' @param backendJS Path to the backend recording script. Downloaded with wwwPrep as "www/rec_backend.js"
#'
#' @return Adds the JS scripts for audio recording to the ui object.
#' @export
#'
#' @examples
useRecorder <- function(frontendJS = "www/rec_frontend.js",
                        backendJS = "www/rec_backend.js") {
  ui <- shiny::tagList(
    shiny::includeScript(frontendJS),
    shiny::includeScript(backendJS)
  )

  return(ui)
}
