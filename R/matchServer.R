#' Matching Game Server Function
#'
#' @param id
#' @param counter
#' @param triggerInit
#' @param triggerReturn
#' @param result
#' @param n2find
#' @param n_items
#' @param n_cols
#' @param n_rows
#' @param items
#' @param lab_type
#' @param randomTarget
#' @param randomGrid
#' @param size
#' @param color
#' @param fill
#'
#' @return
#' @export
#'
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
matchServer <- function(id = "game",
                        counter,
                        triggerInit,
                        triggerReturn,
                        result = c("hide", "disable"),
                        n2find,
                        n_items = 24,
                        n_cols = 4,
                        n_rows = 6,
                        items = "default",
                        lab_type = c("icon", "text"),
                        randomTarget = TRUE,
                        randomGrid = FALSE,
                        size = 2,
                        color = "default",
                        fill = "default") {

  if (items == "default") {
    items <- sample(dest_icons, n_items)
  }

  if (n_cols > 12) {
    n_cols <- 12
    n_rows <- n_items/n_cols
  }

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

  shiny::moduleServer(
    id = id,
    server <- function(input, output, session) {
      ns <- session$ns
      rvs <- shiny::reactiveValues(score = 0, clicked = NA)
      shiny::observeEvent(triggerInit(), {
        shinyjs::showElement("matchdiv")
      })

      shiny::observe({
        shiny::req(counter())

        output$target <- shiny::renderUI({
          shiny::tagList(
            if (lab_type == "icon") {
              actionButton(inputId = ns("target"),
                           label = character(0),
                           icon = shiny::icon(paste0(targs$items[counter()])),
                           style = paste0("background-color:",
                                          targs$fills[counter()], "; color:",
                                          targs$cols[counter()],
                                          "; font-size:", size, "00%"))
            } else if (lab_type == "text") {
              actionButton(inputId = ns(target),
                           label = paste0(targs$items[counter()]),
                           style = paste0("background-color:",
                                          targs$fills[counter()], "; color:",
                                          targs$cols[counter()],
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

          if (rvs$clicked == targs$items[counter()]) {
            rvs$score <- rvs$score + 1
            shinyWidgets::updateProgressBar(session = session,
                                            id = "score",
                                            value = rvs$score,
                                            total = n2find,
                                            status = "success")
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

    observe({
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
      i_df = i_df
    ))
    }
  )

}

