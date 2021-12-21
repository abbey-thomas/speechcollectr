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
