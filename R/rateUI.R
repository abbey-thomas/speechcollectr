#' User interface builder for likert scale tasks
#'
#' @param id The id of the module. Must be the same as the ID of `rateServer()`.
#' @param align One of 'left', 'center', or 'right'. Should the elements in this UI be left-, center-, or right-aligned?
#' @param scaleFillCol A valid hexidecimal code or color name for the scale background. Defaults to white.
#' @param scaleTextCol A valid hexidecimal code or color name for the scale text and border. Defaults to black.
#' @param submitText Character. Label for the submit button. Defaults to 'SUBMIT'.
#' @param submitFillCol A valid hexidecimal code or color name for the submit button background.
#' @param submitTextCol A valid hexidecimal code or color name for the submit button text.
#'
#' @return A shiny UI for likert scale tasks.
#' @export
#' @examples
#' if (interactive()){
#'   library(shiny)
#'   library(shinyjs)
#'   ui <- fluidPage(
#'     actionButton("btn", "Click me"),
#'     rateUI(id = "example"),
#'     textOutput("confirmation")
#'   )
#'
#'   server <- function(input, output, session) {
#'     observeEvent(input$btn, {
#'       hideElement("btn")
#'     })
#'     rating <- rateServer(id = "example",
#'                          trigger = reactive(input$btn),
#'                          instructions = "What do you think?",
#'                          answers = c("Strongly disagree", "Disagree",
#'                                      "Neutral", "Agree", "Strongly agree"),
#'                          pretext = "The vowels 'aw' and 'ah' sound exactly the same.")
#'     observe({
#'       if (isTruthy(rating())) {
#'         output$confirmation <- renderText({
#'           paste0("You selected ", rating(),".")})
#'       }
#'     })
#'   }
#'   shinyApp(ui = ui, server = server)
#' }
#'
#' # An example with 2 scales....
#' if (interactive()){
#'   library(shiny)
#'   library(shinyjs)
#'   ui <- fluidPage(
#'     actionButton("btn", "Click me"),
#'     rateUI(id = "example"),
#'     textOutput("confirmation")
#'   )
#'
#'   server <- function(input, output, session) {
#'     observeEvent(input$btn, {
#'       hideElement("btn")
#'     })
#'     rating <- rateServer(id = "example",
#'                          trigger = reactive(input$btn),
#'                          instructions = "Finish the sentence:",
#'                          answers = list(c("Sound completely the same",
#'                                           "Sound similar, but not totally alike",
#'                                           "Sound pretty different",
#'                                           "Sound totally different"),
#'                                         c("Are produced in the exact same way",
#'                                           "Are produced similarly",
#'                                           "Are produced pretty distinctly",
#'                                           "Are produced in totally distinct ways")),
#'                          pretext = "The vowels 'aw' and 'ah'...",
#'                          n_scales = 2,
#'                          answer_all = TRUE,
#'                          direction = "vertical",
#'                          scale_labs = c("perception", "production"))
#'     observe({
#'       if (isTruthy(rating())) {
#'         output$confirmation <- renderText({
#'           paste0("You selected ", rating(),".")})
#'       }
#'     })
#'   }
#'   shinyApp(ui = ui, server = server)
#' }
rateUI <- function(id = "rate",
                   align = "center",
                   n_scales = 1,
                   scaleFillCol = "white",
                   scaleTextCol = "black",
                   submitText = "SUBMIT",
                   submitFillCol = "white",
                   submitTextCol = "black") {
  ns <- shiny::NS(id)

  ui <- shiny::tagList(
    shinyjs::useShinyjs(),
    if (type == "button") {
      lapply(1:n_scales, function(i) {
        shiny::tags$style(shiny::HTML(paste0(".btn-likert", i,"{background-color:",
                                             col2hex(scaleFillCol[i]), "; color:", col2hex(scaleTextCol[i]),
                                             "; border-color:", col2hex(scaleTextCol[i]),
                                             "; white-space:normal; font-size:20px}")))
      })
    } else {
      shinyWidgets::setSliderColor(scaleFillCol, 1:n_scales)
    },
    shinyjs::hidden(
      shiny::tags$div(id = ns("rate_div"),
                      style = paste0("text-align:", align,";"),
                      #shiny::uiOutput(ns("stim")),
                      #shiny::tags$br(),
                      shiny::uiOutput(ns("likert")),
                      shiny::tags$br(),
                      shinyjs::hidden(shiny::actionButton(ns("submit"),
                                             label = submitText,
                                             style = paste0("color: ", col2hex(submitTextCol),
                                                            "; background-color: ",
                                                            col2hex(submitFillCol)))))
    )
  )
  return(ui)
}
