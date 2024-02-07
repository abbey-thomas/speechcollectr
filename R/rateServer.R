#' Actions for likert scales
#'
#' @param id The id of the module. Must be the same as the ID of `rateUI()`.
#' @param type One of "button" or "slider". Must be the same as the `type` of `rateUI()`.
#' @param trigger If not `NULL` (the default), a reactive expression returning the event that should trigger the appearance of the instructions.
#' @param wait Integer. How long should we wait to display the scale(s) after the trigger event occurs? In milliseconds.
#' @param n_scales Integer. The number of scales to be displayed on the page. Must match the value given in the corresponding call to `rateUI()`
#' @param answer_all If `n_scales` > 1, does the participant need to give a response on each scale or only one of them?
#' @param choices A list with length `n_scales`. The answer choices for each scale should be a character vector in this list. If `type="slider"`, each scale must have EXACTLY 2 answers (the first representing the lower extreme and the second representing the higher extreme). If `n_scales` > 1 and the options are different for each scale, the character vectors should be wrapped in a list.
#' @param instructions Instructions that will appear at the top of the page.
#' @param pretext Character. The text that will appear above the likert scale.
#' @param scale_labs Character. The individual label that will appear above each scale if `n_scales` > 1.
#' @param posttext Character. The text that will appear below the likert scale.
#' @param direction Either "horizontal" or "vertical". Describes the position of the scale options (NOT the scales themselves) relative to each other.
#' @param sliderInit Numeric. The starting value of the slider. Must be between `sliderMin` and `sliderMax`. Default is in the middle of the scale.
#' @param sliderMin Numeric. The lower of the slider. Default is `0`.
#' @param sliderMax Numeric. The upper limit of the slider. Default is `100`.
#' @param pips List with two elements. Where to place numeric markers on the slider scale. See Details for more information.
#' @param step Numeric. The distance between consecutive points (i.e., potential answers) on the slider. Default is `.01`,
#'
#' @return Returns a reactive vector of the answers selected from each scale when the participant clicks submit.
#' @details Notes on sliders:
#' \itemize{
#' \item Consider carefully whether you need a slider. Do you expect participant responses to fall on a continuous scale in meaningful ways, or do you expect answers to fall in a number of blocks along the slider? If the latter, consider using a button scale as they are easier for participants to use (See Toepoel & Funke, 2018 and Chyung et al., 2018 (references below) for more information.).
#' \item Placing numeric markers along the scale and displaying the numeric response values are best for scales where the answer is truly numeric. (i.e., "How tall (in inches) do you think the talker in this recording is?") (See Gummer & Kunz, 2021 (reference below) for more information.)
#' \item The argument pips should be a list with 2 values: The first value, "density", indicates how far apart the minor divisions on the scale should be (e.g., `density=5` will place a marker every 5 integers between `sliderMin` and `sliderMax`). The second element in the list, "values", is a vector denoting where the major breaks of the scale and their numeric labels should go (e.g., if `values=c(0, 50, 100)` markers and numeric labels will be placed at 0, 50, and 100.) All values in `values` must fall between `sliderMin` and `sliderMax`.
#' \item The default starting answer is in the center of the scale, as initializing the handle at the left of the scale has been shown to bias the answers towards the lower values. (CITE)
#' \item Currently, the `rate*()` functions only support text values at the beginning and end of the slider. These will be placed inline with the slider and should represent the two extreme values (low and high, in that order). To place text at only one or the other extreme, just use an empty text string (`""`) for the end of the scale where you do not wish to show a value.
#' }
#'
#' @references
#' Chyung, S. Y., Swanson, I., Roberts, K., & Hankinson, A. (2018). Evidence‐based survey design: The use of continuous rating scales in surveys. \emph{Performance Improvement, 57}(5), 38-48.\cr
#' Gummer, T., & Kunz, T. (2021). Using only numeric labels instead of verbal labels: Stripping rating scales to their bare minimum in web surveys. \emph{Social Science Computer Review, 39}(5), 1003-1029.\cr
#' Toepoel, V., & Funke, F. (2018). Sliders, visual analogue scales, or buttons: Influence of formats and scales in mobile and desktop surveys. \emph{Mathematical Population Studies, 25}(2), 112-122.
#'
#' @export
#' @examples
#' if (interactive()){
#'   library(shiny)
#'   library(shinyjs)
#'   ui <- fluidPage(
#'     actionButton("btn", "Click me"),
#'     rateUI(id = "example",
#'            type = "button"),
#'     actionButton("submit", "SUBMIT"),
#'     textOutput("confirmation")
#'   )
#'
#'   server <- function(input, output, session) {
#'   rvs <- reactiveValues(rating = NULL)
#'     observeEvent(input$btn, {
#'       disable("btn")
#'
#'       rvs$ans <- rateServer(id = "example",
#'                                type = "button",
#'                                instructions = "What do you think?",
#'                                answers = c("Strongly disagree", "Disagree",
#'                                            "Neutral", "Agree", "Strongly agree"),
#'                                pretext = "The vowels 'aw' and 'ah' sound exactly the same.")
#'     })
#'
#'     observeEvent(input$submit, {
#'     enable("btn")
#'         output$confirmation <- renderText({
#'           paste0("You selected ", rvs$ans$ratings[1],".")})
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
#'      actionButton("submit", "SUBMIT"),
#'      textOutput("confirmation")
#'    )
#'
#'    server <- function(input, output, session) {
#'      rvs <- reactiveValues(rating = NULL)
#'
#'      observeEvent(input$btn, {
#'        disable("btn")
#'
#'        rvs$ans <- rateServer(id = "example",
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
#'      observeEvent(input$submit, {
#'        enable("btn")
#'        output$confirmation <- renderText({
#'          paste0("You selected '", rvs$ans$ratings[1],"' and '",  rvs$ans$ratings[2], "'.")})
#'      })
#'    }
#'    shinyApp(ui = ui, server = server)
#'  }
rateServer <- function(id = "rate",
                       trigger = NULL,
                       wait = 1,
                       type = c("button", "slider"),
                       n_scales = 1,
                       answer_all = FALSE,
                       choices = NULL,
                       instructions = "",
                       pretext = "",
                       scale_labs = NULL,
                       posttext = "",
                       direction = "horizontal",
                       sliderInit = 50,
                       sliderMin = 0,
                       sliderMax = 100,
                       pips = NULL,
                       step = .01) {
  if (is.list(choices)) {
    if (length(choices) != n_scales) {
      stop("Argument 'answers' must be a list the same length as 'n_scales', containing a vector of choices for each scale.")
    } else {
      answers <- choices
    }
  } else {
    answers <- lapply(seq_along(1:n_scales), function(x) {c(choices)})
  }

  if (all(!is.null(scale_labs))) {
    if (length(scale_labs) != n_scales)
      stop("Argument 'scale_labs' must either be NULL or a list the same length as 'n_scales', containing a label for each scale.")
  }

  if (type == "slider") {
    if (!is.null(pips)) {
      if (length(pips) != 2)
        stop("Argument 'pips' must be a list of length=2. ")
    }
  }

  scale_labs <- if (!is.null(scale_labs)) {
    as.list(scale_labs)
  } else {
    as.list(rep.int("", times = n_scales))
  }

  colspan <- if (type == "slider"){
    ifelse(direction == "vertical", n_scales, 3)
  } else {
    ifelse(direction == "vertical", n_scales, 1)
  }

  pips <- if (!is.null(pips)) {
    if (length(names(pips))==0) {
      names(pips) <- c("density", "values")
    }

    list(mode = "values",
         density = pips[["density"]],
         values = pips[["values"]])
  }

  shiny::moduleServer(id = id, function(input, output, session) {
    ns <- session$ns

    rate_rvs <- shiny::reactiveValues(sel = character(length = n_scales))
    returns <- shiny::reactiveValues()
    if (is.null(trigger)) {
      trigger <- shiny::reactive(1)
    }

    shiny::observeEvent(trigger(), {
      rate_rvs$sel <- NULL
      if (type == "slider") {
        rate_rvs$sliderInit <- sliderInit
      }
    }, priority = 2)

    shiny::observeEvent(trigger(), {

      shinyjs::delay(wait, shinyjs::showElement("scaleDiv"))
      output$likert <- shiny::renderUI({
        shiny::tags$table(id = "tab",
                          shiny::tags$tr(shiny::tags$th(colspan = colspan, style = "text-align:center;", shiny::h5(instructions))),
                          shiny::tags$tr(shiny::tags$th(colspan = colspan, style = "text-align:center;", shiny::h3(pretext))),
                          if (direction=="vertical") {

                            if (type == "slider") {
                              shiny::tags$tr(
                                lapply(seq_along(c(1:n_scales)), function(i){
                                  shiny::tags$td(style = "padding: 15px; text-align: center; white-space: normal;",
                                                 shiny::tags$h6(ifelse(!is.null(answers), paste0(answers[[i]][2]), "")))
                                })
                              )
                              shiny::tags$tr(
                                lapply(seq_along(c(1:n_scales)), function(i){
                                  shiny::tags$td(style = "padding: 15px;",
                                                 shinyWidgets::noUiSliderInput(inputId = ns(paste0("scale", i)),
                                                                               min = sliderMin,
                                                                               max = sliderMax,
                                                                               value = rate_rvs$sliderInit,
                                                                               tooltips = FALSE,
                                                                               step = step,
                                                                               update_on = "end",
                                                                               orientation = "vertical",
                                                                               pips = pips)
                                  )
                                })
                              )

                              shiny::tags$tr(
                                lapply(seq_along(c(1:n_scales)), function(i){
                                  shiny::tags$td(style = "padding: 15px; text-align: center; white-space: normal;",
                                                 shiny::tags$h6(ifelse(!is.null(answers), paste0(answers[[i]][1]), "")))

                                })
                              )

                            } else {
                              shiny::tags$tr(
                                lapply(seq_along(c(1:n_scales)), function(i){
                                  shiny::tags$td(style = "padding: 15px;",
                                                 shinyWidgets::radioGroupButtons(inputId = ns(paste0("scale", i)),
                                                                                 label = scale_labs[[i]],
                                                                                 choiceNames = answers[[i]],
                                                                                 choiceValues = answers[[i]],
                                                                                 width = "100%",
                                                                                 selected = character(),
                                                                                 checkIcon = list(yes = shiny::icon("check")),
                                                                                 justified = TRUE,
                                                                                 direction = "vertical",
                                                                                 status = paste0("likert", i)))
                                })
                              )
                            }
                          } else {
                            if (type == "slider") {
                              lapply(seq_along(c(1:n_scales)), function(i) {
                                shiny::tags$tr(shiny::tags$td(style = "padding: 15px; text-align: center; white-space: normal;",
                                                              shiny::tags$h6(ifelse(!is.null(answers), paste0(answers[[i]][1]), ""))),
                                               shiny::tags$td(style = "padding: 15px;",
                                                              shinyWidgets::noUiSliderInput(inputId = ns(paste0("scale", i)),
                                                                                            min = sliderMin,
                                                                                            max = sliderMax,
                                                                                            value = rate_rvs$sliderInit,
                                                                                            tooltips = FALSE,
                                                                                            step = step,
                                                                                            update_on = "end",
                                                                                            orientation = "horizontal",
                                                                                            pips = pips)),
                                               shiny::tags$td(style = "padding: 15px; text-align: center; white-space: normal;",
                                                              shiny::tags$h6(ifelse(!is.null(answers), paste0(answers[[i]][2]), ""))))
                              })
                            } else {
                              lapply(seq_along(c(1:n_scales)), function(i) {
                                shiny::tags$tr(shiny::tags$td(shinyWidgets::radioGroupButtons(inputId = ns(paste0("scale", i)),
                                                                                              label = scale_labs[[i]],
                                                                                              choiceNames = answers[[i]],
                                                                                              choiceValues = answers[[i]],
                                                                                              width = "100%",
                                                                                              selected = character(),
                                                                                              checkIcon = list(yes = shiny::icon("check")),
                                                                                              justified = TRUE,
                                                                                              direction = "horizontal",
                                                                                              status = paste0("likert", i))))
                              })
                            }

                          },
                          shiny::tags$tr(shiny::tags$td(colspan = colspan, style = "text-align:center;", shiny::h2(posttext))))
      })
    })
    if (isTRUE(answer_all)) {
      lapply(seq_along(c(1:n_scales)), function(i){
        shiny::observeEvent(input[[paste0("scale", i)]], {
          rate_rvs$sel[i] <- input[[paste0("scale", i)]]
        })
      })

      if (type == "slider") {
        shiny::observe({
          if (all(n_scales != sliderInit)) {
            returns$ratings <- rate_rvs$sel
          }
        })
      } else {
        shiny::observe({
          if (all(nzchar(rate_rvs$sel))) {
            returns$ratings <- rate_rvs$sel
          }
        })
      }
    } else {
      if (type == "slider") {
        lapply(seq_along(c(1:n_scales)), function(i){
          shiny::observeEvent(input[[paste0("scale", i)]], {
            if (input[[paste0("scale", i)]] != sliderInit) {
              rate_rvs$sel[i] <- input[[paste0("scale", i)]]

              lapply(seq_along(c(1:n_scales)), function(j) {
                if (j != i) {
                  if (shiny::isTruthy(input[[paste0("scale", j)]])) {
                    shinyWidgets::updateNoUiSliderInput(session,
                                                        inputId = ns(paste0("scale", j)),
                                                        value = rate_rvs$sliderInit,
                                                        color = "#D3D3D3")
                    rate_rvs$sel[j] <- ""
                  }
                }
              })
              returns$ratings <- rate_rvs$sel[i]

            }
          })
          shiny::observeEvent(input[[paste0("scale", i)]], {
            rate_rvs$sel[i] <- input[[paste0("scale", i)]]
          })
        })
      } else {
        lapply(seq_along(c(1:n_scales)), function(i){
          shiny::observeEvent(input[[paste0("scale", i)]], {
            rate_rvs$sel[i] <- input[[paste0("scale", i)]]

            lapply(seq_along(c(1:n_scales)), function(j) {
              if (j != i) {
                if (shiny::isTruthy(input[[paste0("scale", j)]])) {
                  shinyWidgets::updateRadioGroupButtons(session,
                                                        inputId = ns(paste0("scale", j)),
                                                        selected = character())
                  rate_rvs$sel[j] <- ""
                }
              }
            })

          })
        })
      }

    }

    return(returns)
  })
}
