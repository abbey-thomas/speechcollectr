#' Matching Game Server Function
#'
#' @param id The input ID associated with the matching game module. Must match the ID of `matchUI()`.
#' @param counter The reactive value where we should store the number of matches the player has found so far.
#' @param triggerInit The reactive value that triggers the initial appearance of the matching game. May be an `input$...` value from outside the module wrapped in `reactive()`.
#' @param triggerReturn The reactive value or conditional statement that triggers the reappearance of the matching game UI. May be an `input$...` value from outside the module wrapped in `reactive()`.
#' @param result A character value describing what should happen when the participant finds a match. Must be either "disable" or "hide".
#' @param time_df
#' @param n2find Integer. How many items must the participant find in total?
#' @param n_items Integer. How many items should be displayed? Should be evenly divisible by `n_cols`
#' @param n_cols Integer. How many columns should the grid have?
#' @param items Character. A list of items (either Font Awesome icons) or words to use as labels for the buttons. Default select a random sample of length `n_items` from a list of 100 free Font Awesome icons. If not using "default", vector must be the same length as `n_items`.
#' @param lab_type Are the items text labels or icons? Must be either "text" or "icon"
#' @param randomTarget Boolean. Should the targets be selected from the list randomly (`TRUE`) or in a particular order (`FALSE`)?
#' @param randomGrid Boolean. Should the grid of items be randomized for each participant?
#' @param size An integer between 1 and 3 indicating the size of the matching game buttons (larger = bigger)
#' @param color A single hex code or vector of hex codes indicating the color of the icons or text. Default is black.
#' @param fill A single hex code or vector of hex codes indicating the fill of the matching game buttons. Defaults to values from the "Bright" colorblind friendly palette from Paul Tol (see link in references below).
#'
#' @return A list with two items: (1) score = the number of matches the participant has found so far, and (2) i_df = a data.frame containing the list of items used in the present game and their order.
#' @seealso Must be used with \code{\link{matchUI}}.
#' @references Paul Tol's colorblind-safe palettes (the source of the default button colors) can be found at \url{https://personal.sron.nl/~pault/#sec:qualitative}.
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
#' @importFrom timeR start
#' @importFrom timeR stop
#' @importFrom timeR getEvent
matchServer <- function(id = "game",
                        counter,
                        triggerInit,
                        triggerReturn,
                        result = c("hide", "disable"),
                        n2find,
                        n_items = 24,
                        n_cols = 4,
                        items = "default",
                        lab_type = c("icon", "text"),
                        randomTarget = TRUE,
                        randomGrid = FALSE,
                        size = 2,
                        color = "default",
                        fill = "default") {

  if (items == "default") {
    items <- sample(dest_icons, n_items)
  } else {
    if (length(items) != n_items) stop("The number of labels in items must match n_items.")
  }

  if (n_cols > 12) {
    n_cols <- 12
  }

  if (n_items%%n_cols != 0) {
    divs <- seq_len(n_items)
    divs <- divs[n_items %% divs == 0]
    divs <- divs[divs <= 12]
    rems <- which.min(abs(divs-n_cols))
    n_cols <- divs[rems]
  }

  n_rows <- n_items/n_cols

  if ((n_cols*size)>12) {
    size <- 12/n_cols
  }

  if (fill == "default") {
    fills <- rep_len(c("#4477AA", "#66CCEE", "#228833",
              "#CCBB44", "#EE6677", "#AA3377"),
              length.out = n_items)
  } else {
    fills <- rep_len(c(fill), length.out = n_items)
  }

  if (color == "default") {
    cols <- rep_len(c("#000000"), length.out = n_items)
  } else {
    cols <- rep_len(c(color), length.out = n_items)
  }

  i_df <- data.frame(items=c(items),
                     fills = c(fills),
                     cols = c(cols))

  if (isTRUE(randomTarget)) {
    targs <- i_df[sample(nrow(i_df)),]
  } else {
    targs <- i_df
  }

  if (isTRUE(randomGrid)) {
    g_df <- i_df[sample(nrow(i_df)),]
  } else {
    g_df <- i_df
  }

  ord <- sort(rep(c(1:n_cols), n_rows))
  i_list <- split(g_df, ord)
  rt <- timeR::createTimer(verbose = FALSE, precision = "ms")

  shiny::moduleServer(
    id = id,
    server <- function(input, output, session) {
      ns <- session$ns
      rvs <- shiny::reactiveValues(score = 0, clicked = NA)
      times <- shiny::reactiveVal(value = data.frame(event = character(),
                                                    start = character(),
                                                    end = character(),
                                                    timeElapsed = numeric(),
                                                    comment = character()))
      shiny::observeEvent(triggerInit(), {
        shinyjs::showElement("matchdiv")
        rt$start(paste0("trial", (rvs$score+1)))
      })

      shiny::observe({
        shiny::req(counter())

        output$target <- shiny::renderUI({
          shiny::tagList(
            if (lab_type == "icon") {
              actionButton(inputId = ns("target"),
                           label = character(0),
                           icon = shiny::icon(paste0(targs$items[as.numeric(counter())])),
                           style = paste0("background-color:",
                                          targs$fills[as.numeric(counter())], "; color:",
                                          targs$cols[as.numeric(counter())],
                                          "; font-size:", size, "00%"))
            } else if (lab_type == "text") {
              actionButton(inputId = ns(target),
                           label = paste0(targs$items[as.numeric(counter())]),
                           style = paste0("background-color:",
                                          targs$fills[as.numeric(counter())], "; color:",
                                          targs$cols[as.numeric(counter())],
                                          "; font-size:", size, "00%;"))
            }
          )
        })

        output$grid <- shiny::renderUI({
          shiny::tagList(
            shiny::fluidRow(
              shiny::column(width = 12,
                            offset = (12-(n_cols*size))/2,
                            lapply(c(1:n_cols), function(c){
                              shiny::column(width = size, align = "center",
                                            lapply(c(1:n_rows), function(i){
                                              shiny::actionButton(inputId = ns(paste0(i_list[[c]]$items[i])),
                                                                  label = character(0),
                                                                  icon = shiny::icon("question-circle"),
                                                                  style = paste0("background-color:",
                                                                                 i_list[[c]]$fills[i], "; color:",
                                                                                 i_list[[c]]$cols[i],
                                                                                 "; font-size:", size, "00%;"),
                                                                  class = "match-grid")
                                            })
                              )
                            })
              )
            )
          )
        })
      })

      lapply(seq_along(items), function(i){
        shiny::observeEvent(input[[paste0(items[i])]], {
          shiny::updateActionButton(session = session,
                                    inputId = paste0(items[i]),
                                    label = character(0),
                                    icon = shiny::icon(paste0(items[i])))

          rvs$clicked <- paste0(items[i])

          shinyjs::delay(1000, shiny::updateActionButton(session = session,
                                                         inputId = paste0(items[i]),
                                                         label = character(0),
                                                         icon = shiny::icon("question-circle")))

          if (rvs$clicked == targs$items[as.numeric(counter())]) {
            rt$stop(paste0("trial", (rvs$score+1)))
            times(bind_rows(times(), rt$getEvent(paste0("trial", (rvs$score+1)))))

            if (rvs$score < n2find) {
              rvs$score <- rvs$score + 1
              shinyWidgets::updateProgressBar(session = session,
                                              id = "score",
                                              value = rvs$score,
                                              total = n2find,
                                              status = "success")
            } else {
              shinyalert::shinyalert(
                title = "Congratulations!",
                text = "You found all the matches!",
                type = success)
            }

            if (result == "hide") {
              shinyjs::delay(1500,
                             shinyjs::hide("matchdiv"))
            } else if (result == "disable") {
              shinyjs::disable("matchdiv")
            }
          } else {
            shinyWidgets::updateProgressBar(session = session,
                                            id = "score",
                                            value = rvs$score,
                                            total = n2find,
                                            status = "danger")
          }
        })
      })

    observeEvent(triggerReturn(), {
      rt$start(paste0("trial", (rvs$score+1)))
      if (result == "hide") {
        shinyjs::toggleElement(id = "matchdiv",
                               condition = triggerReturn())
      } else {
        shinyjs::toggleState(id = "matchdiv",
                             condition = triggerReturn())
      }
    })

    return(list(
      n_found = shiny::reactive(rvs$score),
      i_df = i_df,
      time_df = shiny::reactive(times())
    ))
    }
  )

}

