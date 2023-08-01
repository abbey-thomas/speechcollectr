#' User interface builder for likert scale tasks
#'
#' @param id The id of the module. Must be the same as the ID of `rateServer()`.
#' @param type One of 'button' or 'slider'. Which type of input will the respondents give? Must be the same as in the corresponding call to `rateServer()`.
#' @param align One of 'left', 'center', or 'right'. Should the elements in this UI be left-, center-, or right-aligned?
#' @param n_scales Integer. How many scales should be displayed in the interface? Must be the same value as in the corresponding call to `rateServer()`
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
#'     rateUI(id = "example",
#'            type = "button"),
#'     textOutput("confirmation")
#'   )
#'
#'   server <- function(input, output, session) {
#'   rvs <- reactiveValues(rating = NULL)
#'     observeEvent(input$btn, {
#'       disable("btn")
#'
#'       rvs$rating <- rateServer(id = "example",
#'                                type = "button",
#'                                instructions = "What do you think?",
#'                                answers = c("Strongly disagree", "Disagree",
#'                                            "Neutral", "Agree", "Strongly agree"),
#'                                pretext = "The vowels 'aw' and 'ah' sound exactly the same.")
#'     })
#'
#'     observeEvent(input[["example-submit"]], {
#'     enable("btn")
#'         output$confirmation <- renderText({
#'           paste0("You selected ", rvs$rating(),".")})
#'
#'     })
#'   }
#'   shinyApp(ui = ui, server = server)
#' }
#'
#' # An example with 2 scales....
#' if (interactive()){
#'    library(shiny)
#'    library(shinyjs)
#'    ui <- fluidPage(
#'      actionButton("btn", "Click me"),
#'      rateUI(id = "example",
#'             type = "button",
#'             n_scales = 2),
#'      textOutput("confirmation")
#'    )
#'
#'    server <- function(input, output, session) {
#'      rvs <- reactiveValues(rating = NULL)
#'
#'      observeEvent(input$btn, {
#'        disable("btn")
#'
#'        rvs$rating <- rateServer(id = "example",
#'                                 trigger = NULL,
#'                                 type = "button",
#'                                 instructions = "Finish the sentence:",
#'                                 answers = list(c("Sound completely the same",
#'                                                  "Sound similar, but not totally alike",
#'                                                  "Sound pretty different",
#'                                                  "Sound totally different"),
#'                                                c("Are produced in the exact same way",
#'                                                  "Are produced similarly",
#'                                                  "Are produced pretty distinctly",
#'                                                  "Are produced in totally distinct ways")),
#'                                 pretext = "The vowels 'aw' and 'ah'...",
#'                                 n_scales = 2,
#'                                 answer_all = TRUE,
#'                                 direction = "vertical",
#'                                 scale_labs = c("perception", "production"))
#'      })
#'
#'      observeEvent(input[["example-submit"]], {
#'        enable("btn")
#'        output$confirmation <- renderText({
#'          paste0("You selected '", rvs$rating()[[1]],"' and '",  rvs$rating()[[2]], "'.")})
#'      })
#'    }
#'    shinyApp(ui = ui, server = server)
#'  }
rateUI <- function(id = "rate",
                   type = c("button", "slider"),
                   align = "center",
                   n_scales = 1,
                   scaleFillCol = "white",
                   scaleTextCol = "black",
                   scaleTextSz = "20px",
                   submitText = "SUBMIT",
                   submitFillCol = "white",
                   submitTextCol = "black") {

  fills <- col2hex(rep(c(scaleFillCol), length.out = n_scales))
  texts <- col2hex(rep(c(scaleTextCol), length.out = n_scales))

  ui <- shiny::tagList(
    shinyjs::useShinyjs(),
    lapply(1:n_scales, function(i) {
      if (type == "button") {
        shiny::tags$style(shiny::HTML(paste0(".btn-likert", i,"{background-color:",
                                             fills[i], "; color:", texts[i],
                                             "; border-color:", texts[i],
                                             "; white-space:normal;
                                             font-size:", scaleTextSz, "}")))
      } else {
        shiny::tags$style(shiny::HTML(paste0("#", id, "-scale", i,
                                             ".noUi-connect {background:",
                                             fills[i], ";}")))
      }

    }),
    shinyjs::hidden(
      shiny::tags$div(id = id,
                      style = paste0("text-align:", align,";"),
                      shiny::uiOutput(outputId = paste0(id, "-likert")),
                      shiny::tags$br(),
                      shinyjs::hidden(shiny::actionButton(paste0(id, "-submit"),
                                             label = submitText,
                                             style = paste0("color: ", col2hex(submitTextCol),
                                                            "; background-color: ",
                                                            col2hex(submitFillCol)))))
    )
  )
  return(ui)
}
