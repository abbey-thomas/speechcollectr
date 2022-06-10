#' A User Interface for quick T/F questions about a participant's qualifications or environment
#'
#' @param id The module ID. Must be the same as the ID of `checkServer()`.
#' @param title Character. Usually, the title of the experiment. But can be anything else you want to be printed in large friendly letters at the top of the page.
#' @param type Either "participant" or "environment", to specify whether you want to ask questions about a participant's qualifications (i.e., do they meet inclusion criteria) or their acoustic environment (i.e., is it quiet enough for speech research).
#' @param instructions What should the participant do when the TRUE/FALSE question appears?
#' @param align One of 'left', 'center', or 'right'. Should the elements in this UI be left-, center-, or right-aligned?
#'
#' @return User interface elements for TRUE/FALSE questions, displayed one at a time.
#' @export
#' @examples
#'  # First get some sample questions for your participant.
#' data("qualifications")
#' write.csv(qualifications, "qualifications.csv", row.names = FALSE)
#'
#' # Now ask the questions!
#' if (interactive()) {
#'   shinyApp(
#'     ui = fluidPage(
#'       fluidRow(
#'         column(width = 8, offset = 2,
#'                actionButton("btn", "Click me"),
#'                checkUI(id = "example", title = "Speech Experiment",
#'                        type = "participant"),
#'                textOutput("confirmation"))
#'       )
#'
#'     ),
#'     server = function(input, output, session) {
#'       answer <- checkServer(id = "example",
#'                             trigger = reactive(input$btn),
#'                             questionFile = "qualifications.csv",
#'                             outFile = NULL,
#'                             returnVals = c("eighteen"))
#'       observeEvent(input$btn, {
#'         shinyjs::hide("btn")
#'       })
#'       observe({
#'         if (isTruthy(answer$eighteen))
#'           output$confirmation <- renderText("This participant is an adult.")
#'       })
#'     }
#'   )
#' }
checkUI <- function(id = "check",
                    title = "",
                    type = c("participant", "environment"),
                    instructions = "default",
                    align = "center"){
  if (type != "participant" & type != "environment")
    stop("Argument 'type' must be either 'participant' for questions about the participant's qualifications) or 'environment' (for questions about the participant's [acoustic] environment).")

  ns <- shiny::NS(id)

  if (instructions == "default"){
    if (type == "participant") {
      instr <- "Please mark whether the following statements are TRUE or FALSE about you."
    } else {
      instr <- "Please mark whether the following statements are TRUE or FALSE with regards to your current environment. You will still be able to complete the experiment if you answer FALSE, but fewer distractions and quieter environments make completion of the experiment easier. "
    }
  } else if (is.null(instructions)) {
    instr <- ""
  } else {
    instr <- as.character(instructions)
  }

  ui <- shiny::tagList(
    shinyalert::useShinyalert(),
    shinyjs::useShinyjs(),
    shinyjs::hidden(
      shiny::tags$div(id = ns("ck_div"),
                      style = paste0("text-align:", align,";"),
                      shiny::tags$h2(id = ns("title"), title),
                      shiny::tags$h4(id = ns("instructions"), instr),
                      shiny::tags$br(),
                      shiny::uiOutput(ns("ck_qs"))))
  )
  return(ui)
}
