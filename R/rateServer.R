rateServer <- function(id = "rate",
                       trigger,
                       result = "hide",
                       wait = 1000,
                       n_scales = 1,
                       answer_all = FALSE,
                       answers,
                       instructions = "",
                       pretext = "",
                       scale_labs = NULL,
                       posttext = "",
                       direction = "horizontal") {
  #if (grepl("\\.wav$",stimulus)){
   # stimType <- "wav"
  #} else {
   # stimType <- "txt"
  #}

  if (n_scales > 1) {
    if (!is.list(answers)|length(answers) != n_scales)
      stop("Argument 'answers' must be a list the same length as 'n_scales', containing a vector of choices for each scale.")
  } else {
    answers <- list(c(answers))
  }

  if (all(!is.null(scale_labs))) {
    if (length(scale_labs) != n_scales)
      stop("Argument 'scale_labs' must either be NULL or a list the same length as 'n_scales', containing a label for each scale.")
  }

  scale_labs <- as.list(scale_labs)

  colspan <- ifelse(direction == "vertical", n_scales, 1)

  shiny::moduleServer(
    id = id,
    function(input, output, session){
      ns <- session$ns
      rvs <- shiny::reactiveValues(sel = character(length = n_scales))

      shiny::observeEvent(trigger(), {
        shinyjs::delay(wait, shinyjs::showElement("rate_div"))
        output$likert <- shiny::renderUI({
          shiny::tags$table(id = "rate_tab",
                     shiny::tags$tr(shiny::tags$th(colspan = colspan, style = "text-align:center;", shiny::h5(instructions))),
                     shiny::tags$tr(shiny::tags$th(colspan = colspan, style = "text-align:center;", shiny::h3(pretext))),
                     if (direction=="vertical") {
                       shiny::tags$tr(
                         lapply(seq_along(c(1:n_scales)), function(i){
                           shiny::tags$td(style = "padding: 15px;",
                                          shinyWidgets::radioGroupButtons(inputId = ns(paste0("scale", i)),
                                                                          label = scale_labs[[i]],
                                                                          choiceNames = answers[[i]],
                                                                          choiceValues = answers[[i]],
                                                                          width = "100%",
                                                                          selected = character(),
                                                                          checkIcon = list(yes = icon("check")),
                                                                          justified = TRUE,
                                                                          direction = "vertical",
                                                                          status = "likert"))
                         })
                       )
                     } else {
                       lapply(seq_along(c(1:n_scales)), function(i) {
                         shiny::tags$tr(shiny::tags$td(shinyWidgets::radioGroupButtons(inputId = ns(paste0("scale", i)),
                                                                                       label = scale_labs[[i]],
                                                                                       choiceNames = answers[[i]],
                                                                                       choiceValues = answers[[i]],
                                                                                       width = "100%",
                                                                                       selected = character(),
                                                                                       checkIcon = list(yes = icon("check")),
                                                                                       justified = TRUE,
                                                                                       direction = "horizontal",
                                                                                       status = "likert")))
                       })
                     },
                     shiny::tags$tr(shiny::tags$td(colspan = colspan, style = "text-align:center;", shiny::h2(posttext))))
        })
      })
      if (isTRUE(answer_all)) {

        lapply(seq_along(c(1:n_scales)), function(i){
          shiny::observeEvent(input[[paste0("scale", i)]], {
            rvs$sel[i] <- input[[paste0("scale", i)]]
          })
        })
        shiny::observe({
          shinyjs::toggleElement("submit",
                                 condition = all(nzchar(rvs$sel)))
        })

        shiny::observeEvent(input$submit, {
          rvs$selected <- rvs$sel
        })
      } else {
        lapply(seq_along(c(1:n_scales)), function(i){
          shiny::observeEvent(input[[paste0("scale", i)]], {
            shinyjs::showElement("submit")
            rvs$sel[i] <- input[[paste0("scale", i)]]

            lapply(seq_along(c(1:n_scales)), function(j) {
              if (j != i) {
                if (shiny::isTruthy(input[[paste0("scale", j)]])) {
                  shinyWidgets::updateRadioGroupButtons(session,
                                                        inputId = paste0("scale", j),
                                                        selected = character())
                  rvs$sel[j] <- ""
                }
              }
            })

            shiny::observeEvent(input$submit, {
              rvs$selected <- rvs$sel[i]
            })
          })
        })
      }

      shiny::observeEvent(input$submit, {
        lapply(seq_along(c(1:n_scales)), function(j) {
          shinyWidgets::updateRadioGroupButtons(session,
                                                inputId = paste0("scale", j),
                                                selected = character())})
        shinyjs::hide("submit")
        shinyjs::hide("rate_div")
      })
      return(shiny::reactive(rvs$selected))
    }
  )
}
