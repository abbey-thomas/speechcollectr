#' Matching Game User Interface
#'
#' @param id The input ID associated with the matching game module. Must match the ID of `matchServer()`.
#' @param title Character. The title that will be displayed at the top of the UI. Defaults to "Find a Match!"
#' @param instructions Character. The instructions that will be displayed above the grid of buttons. Defaults to general instructions for the matching game.
#' @param n2find The number of items a participant must find. Must be the same as `n2find` in `matchServer()`
#'
#' @return A minimal user interface for the matching game that includes a title, instructions, and a progress bar.
#' @seealso Must be used with \code{\link{matchServer}}.
#' @export
#' @examples
#' library(shiny)
#' library(shinyjs)
#'
#' ui <- fluidPage(
#'   fluidRow(
#'     column(width = 8, offset = 2,
#'            actionButton("start", "Start"),
#'            hidden(actionButton("again", "Play Again")),
#'            matchUI(n2find = 24)
#'            )
#'   )
#' )
#'
#' server <- function(input, output, session) {
#'   counter <- reactiveValues(n = 1)
#'   matches <- matchServer(triggerInit = reactive(input$start),
#'               triggerReturn = reactive(input$again),
#'               counter = reactive(counter$n),
#'               n2find = 24,
#'               randomGrid = TRUE,
#'               lab_type = "icon",
#'               result = "hide")
#'
#'   observe({
#'     if (matches$n_found() > 0) {
#'       showElement("again")
#'     }
#'   })
#'
#'   observeEvent(input$again, {
#'     counter$n <- counter$n + 1
#'   })
#' }
#'
#' shinyApp(ui = ui, server = server)
#'
matchUI <- function(id = "game",
                    title = "Find a Match!",
                    instructions = "Click a button in the grid below to see the image it is hiding. Keep clicking buttons until you find the image that matches the one above.",
                    n2find) {
  ns <- shiny::NS(id)
  ui <- shiny::tagList(
    shinyjs::useShinyjs(),
    shinyalert::useShinyalert(),
    shinyjs::hidden(shiny::tags$div(id = ns("matchdiv"), style = "text-align:center;",
                                    shiny::tags$h1(id = ns("title"), paste0(title)),
                                    shiny::tags$br(),
                                    shiny::uiOutput(ns("target")),
                                    shiny::tags$h5(id = ns("instr"), paste0(instructions)),
                                    shiny::uiOutput(ns("grid")),
                                    shinyWidgets::progressBar(id = ns("score"),
                                                              value = 0,
                                                              total = n2find)
    ))
  )
}
