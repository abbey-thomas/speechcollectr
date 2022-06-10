#' Create a User Interface for the survey module
#'
#' @param id The module id. Must be the same as \code{\link{surveyServer}}.
#' @param questionFile A valid file path to an existing CSV file, formatted according to the instructions in \code{\link{surveyPrep}}. Must be the same as the one used in \code{\link{surveyServer}}.
#' @param title The title of the survey to display to participants.
#' @param subtitle The subtitle of the survey to display to participants.
#' @param notListedLab For non-textInput questions, the name of the choice that will allow your participants to enter their own value for all questions that include this string in the 'options' column of `questionFile`.
#' @param requiredLab The value that will be appended to required questions' labels. Defaults to asterisk (*).
#' @param submitLab The label that should be displayed on the button participants will click to submit the form.
#'
#' @return A Shiny user interface with the questions specified in the `questionFile` CSV file.
#' @export
#' @details In a Shiny App, this should always be used with \code{\link{surveyServer}}. Before creating the app, be sure to format your `questionFile` according to the instructions in \code{\link{surveyPrep}}.
#'
#' @examples
#' library(shiny)
#' data("demographics")
#' write.csv(demographics, "demographics.csv", row.names = FALSE)
#' # If you use your own survey file, run surveyPrep() first!
#'
#' ui <- fluidPage(
#'   sidebarLayout(
#'     sidebarPanel(width = 4,
#'                  textOutput("age")),
#'     mainPanel(width = 8,
#'               actionButton("begin", "Begin"),
#'               surveyUI(id = "survey",
#'                        title = "Background Information Survey",
#'                        questionFile = "demographics.csv",
#'                        notListedLab = "Not listed:")
#'     )
#'   )
#' )
#' server <- function(input, output, session) {
#'   answers <- surveyServer(id = "survey",
#'                questionFile = questionFile,
#'                notListedLab = "Not listed:",
#'                outFile = "sample.rds",
#'                trigger = reactive(input$begin),
#'                returnVals = c("age", "sex"),
#'                result = "clear")
#'   observeEvent(answers$age, {
#'     output$age <- renderText({paste0("You are ", answers$age, " years old.")})
#'   })
#' }
#' shinyApp(ui = ui, server = server)
surveyUI <- function(id = "survey",
                     questionFile,
                     title = "Survey",
                     subtitle = "Please answer the questions below. Questions marked with * are required.",
                     notListedLab = NULL,
                     requiredLab = "*",
                     submitLab = "SUBMIT") {
  ns <- shiny::NS(id)

  qs <- read.csv(file = questionFile) %>%
    dplyr::mutate(priority = ifelse(priority == "required"|priority == "req", "r",
                                    ifelse(priority == "optional"|priority == "opt", "o",
                                           as.character(priority)))) %>%
    dplyr::mutate(type = ifelse(type == "textInput"|type == "text", "t",
                                ifelse(type == "selectInput"|type == "select", "s",
                                       ifelse(type == "numericInput"|type == "numeric", "n",
                                              ifelse(type == "radioButtons"|type == "radio", "r",
                                                     ifelse(type == "checkboxGroupInput"|type == "checkbox", "c",
                                                            as.character(type))))))) %>%
    dplyr::mutate(across(everything(), .fns = as.character)) %>%
    dplyr::mutate(label = ifelse(priority == "r" & !is.null(requiredLab),
                                 paste0(label, " <span style=color:#CC3311>", requiredLab, "</span>"),
                                 as.character(label)))

  qsl <- setNames(split(qs, seq(nrow(qs))), rownames(qs))
  ui <- shiny::tagList(
    shinyjs::useShinyjs(),
    shinyalert::useShinyalert(),
    shinyjs::hidden(htmltools::tags$div(id = ns("survey_div"),
                        htmltools::tags$h1(id = ns("title"), title),
                        htmltools::tags$h3(id = ns("subtitle"), subtitle),
                        htmltools::tags$br(),
                        lapply(seq_along(qsl), function(i) {
                          if (qsl[[i]]$type != "t") {
                            opts <- gsub(", ", ",", qsl[[i]]$options)
                            opts <- c(unlist(strsplit(opts, ",")))
                          }

                          if (qsl[[i]]$type == "n") {
                            opts <- as.numeric(opts)
                            opts <- c(opts[1]:opts[2])
                          }

                          if (qsl[[i]]$type == "r") {
                            vals <- gsub("[^[:alnum:]]", "", opts)
                          }

                          if (qsl[[i]]$type != "t") {
                            opts <- c("", opts)
                          }

                          if (qsl[[i]]$type == "t") {
                            input <- shiny::textInput(inputId = ns(qsl[[i]]$id),
                                                      label = HTML(qsl[[i]]$label),
                                                      width = '100%')
                          } else if (qsl[[i]]$type == "s"|qsl[[i]]$type == "n") {
                            input <- shiny::selectInput(inputId = ns(qsl[[i]]$id),
                                                        label = HTML(qsl[[i]]$label),
                                                        choices = opts,
                                                        selected = character())
                          } else if (qsl[[i]]$type == "r") {
                            input <- shiny::radioButtons(inputId = ns(qsl[[i]]$id),
                                                         label = HTML(qsl[[i]]$label),
                                                         width = '100%',
                                                         choiceNames = opts,
                                                         choiceValues = vals,
                                                         selected = character())
                          } else {
                            input <- shiny::checkboxGroupInput(inputId = ns(qsl[[i]]$id),
                                                               label = HTML(qsl[[i]]$label),
                                                               width = '100%',
                                                               choiceNames = opts,
                                                               choiceValues = vals,
                                                               selected = character())
                          }

                          return(input)
                        }),
                        shinyjs::disabled(shiny::actionButton(ns("submit"), submitLab)),
                        htmltools::tags$hr()))
  )
  return(ui)
}
