#' Vertically align content at the bottom of the screen
#'
#' @param align One of "center" (default), "left", or "right", describing the horizontal alignment of the content.
#' @param ... Any ui elements
#'
#' @return A user interface with the content wrapped by the function appearing at the bottom of the screen.
#' @export
#'
#' @examples
#' library(shiny)
#' if (interactive()) {
#' ui <- fluidPage(
#'   middle(fluidRow(actionButton("test1", "Test1")),
#'          fluidRow(actionButton("test2", "Test2"))),
#'   bottom("this is at the bottom", align = "right")
#' )
#'
#' server <- function(input, output, session) {
#' }
#' shinyApp(ui, server)
#' }

bottom <- function(..., align = "center") {

  if (align == "left") {
    hpos <- c(95, 5)
  } else if (align == "right") {
    hpos <- c(5, 95)
  } else {
    hpos <- c(50, 50)
  }
  shiny::div(class = "bottom",
             style = paste0("position: fixed;
  bottom: 5px;
  transform: translate(-50%, -", hpos[1], "%);
  width:100%;
  margin: 0 auto; left:", hpos[2], "%;"),
             ...)
}
