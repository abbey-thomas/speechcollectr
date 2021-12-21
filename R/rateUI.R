rateUI <- function(id = "rate",
                   align = "center",
                   scaleFillCol = "white",
                   scaleTextCol = "black",
                   submitText = "SUBMIT",
                   submitFillCol = "white",
                   submitTextCol = "black") {
  ns <- shiny::NS(id)

  ui <- shiny::tagList(
    shinyjs::useShinyjs(),
    shiny::tags$style(shiny::HTML(paste0(".btn-likert{background-color:",
                                         col2hex(scaleFillCol), "; color:", col2hex(scaleTextCol),
                                         "; border-color:", col2hex(scaleTextCol),
                                         "; white-space:normal; font-size:20px}"))),
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
