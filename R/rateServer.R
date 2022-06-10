#' Actions for likert scale tasks
#'
#' @param id The id of the module. Must be the same as the ID of `rateUI()`.
#' @param trigger A reactive value indicating the event that should trigger the appearance of the instructions and (optionally) the play button for the audio stimulus. May be an `input$...` value from outside the module wrapped in `reactive()`.
#' @param wait Integer. How long should we wait to display the scale(s) after the trigger event occurs? If this value will describe the delay between when the participant clicks "play" and when the scale appears. In milliseconds.
#' @param n_scales Integer. The number of scales to be displayed on the page.
#' @param answer_all If `n_scales` > 1, does the participant need to give a response on each scale or only one of them?
#' @param type One of "slider" or "button". What type of scale do you want to display? See \strong{Details} for more information
#' @param range If `type` is "slider", what are the minimum and maximum values of your scale? Defaults to c(0,100).
#' @param unit If `type` is slider,
#' @param increment If `type` is slider,
#' @param display_nums If `type` is slider,
#' @param answer_pos If `type` is slider,
#' @param answers A list with length `n_scales`. The answer choices for each scale should be a character vector in this list. If `n_scales` > 1 and the options are different for each scale, the character vectors should be wrapped in a list.
#' @param instructions Instructions that will appear at the top of the page.
#' @param sound If not NULL (the default), the name of WAV file to rate with the scale.
#' @param n_plays Integer. If sound is not NULL, how many times can the participant play the audio? Defaults to 5.
#' @param play_lab Character. The label for the play button if sound is not NULL. Defaults to "PLAY SOUND".
#' @param pretext Character. The text that will appear above the likert scale.
#' @param scale_labs Character. The individual label that will appear above each scale if `n_scales` > 1.
#' @param posttext Character. The text that will appear below the likert scale.
#' @param direction Either "horizontal" or "vertical". Describes the position of the scale options relative to each other.
#'
#' @return Returns a reactive vector of the answers selected from each scale when the participant clicks submit.
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
rateServer <- function(id = "rate",
                       trigger,
                       wait = 1000,
                       n_scales = 1,
                       answer_all = FALSE,
                       answers,
                       instructions = "",
                       pretext = "",
                       scale_labs = NULL,
                       posttext = "",
                       direction = "horizontal") {
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

  if (type == "slider" & direction == "vertical") {
    stop("Vertical sliders are not supported at this time. Please change the 'direction' argument to 'horizontal' or use buttons.")
  }

  scale_labs <- as.list(scale_labs)

  colspan <- ifelse(direction == "vertical", n_scales, 1)

  shiny::moduleServer(
    id = id,
    function(input, output, session){
      ns <- session$ns
      rvs <- shiny::reactiveValues(sel = character(length = n_scales))

      if (type == "button") {
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
                                                                                   status = paste0("likert", i)))
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
                                                                                                status = paste0("likert", i))))
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
      }

      if (type == "slider") {
        shiny::observeEvent(trigger(), {
          shinyjs::delay(wait, shinyjs::showElement("rate_div"))
          output$likert <- shiny::renderUI({
            shiny::tags$table(id = "rate_tab", style = "width: 100%",
                              shiny::tags$tr(shiny::tags$th(colspan = colspan, style = "text-align:center;", shiny::h5(instructions))),
                              shiny::tags$tr(shiny::tags$th(colspan = colspan, style = "text-align:center;", shiny::h3(pretext))),
                              lapply(seq_along(c(1:n_scales)), function(i) {
                                shiny::tags$tr(shiny::tags$td(style = "width:20%; text-align:center;", shiny::h4(answers[[i]][1])))

                              }),

                              shiny::tags$tr(shiny::tags$td(colspan = colspan, style = "text-align:center;", shiny::h2(posttext))))
          })
        })
      }


      return(shiny::reactive(rvs$selected))
    }
  )
}
