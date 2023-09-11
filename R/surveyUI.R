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
#' library(shinyjs)
#' data("demographics")
#' write.csv(demographics, "demographics.csv", row.names = FALSE)
#' # If you use your own survey file, run surveyPrep() first!
#'
#' # You can use IDs specified in `returnVals` to trigger events after the survey is submitted.
#' if (interactive()) {
#'   ui <- fluidPage(
#'     useShinyjs(),
#'     sidebarLayout(
#'       sidebarPanel(width = 4,
#'                    textOutput("age")),
#'       mainPanel(width = 8,
#'                 actionButton("begin", "Begin"),
#'                 surveyUI(id = "survey",
#'                          title = "Background Information Survey",
#'                          questionFile = "demographics.csv",
#'                          notListedLab = "Not listed:")
#'       )
#'     )
#'   )
#'
#'   server <- function(input, output, session) {
#'     observeEvent(input$begin, {
#'       hide("begin")
#'       # Show the survey when "begin" is clicked.
#'       answers <- surveyServer(id = "survey",
#'                               questionFile = "demographics.csv",
#'                               notListedLab = "Not listed:",
#'                               outFile = "sample.rds",
#'                               returnVals = c("age", "sex"),
#'                               result = "clear")
#'
#'       # Once an answer for "age" is submitted, show the answer in the sidebar panel.
#'       observeEvent(answers$age, {
#'         output$age <- renderText({paste0("You are ", answers$age, " years old.")})
#'       })
#'     })
#'   }
#'   shinyApp(ui = ui, server = server)
#' }
#'
#' # Or, you can trigger events on click of the survey's submit button using the module id in the following string: `input[["id-submit"]]`
#' if (interactive()) {
#'   ui <- fluidPage(
#'     sidebarLayout(
#'       sidebarPanel(width = 4,
#'                    textOutput("age")),
#'       mainPanel(width = 8,
#'                 actionButton("begin", "Begin"),
#'                 surveyUI(id = "survey",
#'                          title = "Background Information Survey",
#'                          questionFile = "demographics.csv",
#'                          notListedLab = "Not listed:")
#'       )
#'     )
#'   )
#'
#'   server <- function(input, output, session) {
#'     observeEvent(input$begin, {
#'       answers <- surveyServer(id = "survey",
#'                               questionFile = "demographics.csv",
#'                               notListedLab = "Not listed:",
#'                               outFile = "sample.rds",
#'                               returnVals = c("age", "sex"),
#'                               result = "hide")
#'
#'       observeEvent(input[["survey-submit"]], {
#'         output$age <- renderText({paste0("You are ", answers$age, " years old.")})
#'       })
#'     })
#'   }
#'   shinyApp(ui = ui, server = server)
#' }
surveyUI <- function (id = "survey",
                      questionFile,
                      title = "Survey",
                      subtitle = "Please answer the questions below. Questions marked with * are required.",
                      requiredLab = "*",
                      submitLab = "SUBMIT")
{
  ns <- shiny::NS(id)
  if (grepl("\\.csv$", questionFile)) {
    qs_pre <- read.csv(file = questionFile)
  } else if (is.data.frame(questionFile)) {
    qs_pre <- questionFile
  } else {
    stop("questionFile must be a valid path to a .csv file or a data.frame in your current R environment!")
  }

  qs_init <- qs_pre %>%
    dplyr::mutate(priority = ifelse(priority ==
                                      "required" | priority == "req", "r",
                                    ifelse(priority == "optional" | priority == "opt", "o",
                                           as.character(priority)))) %>%
    dplyr::mutate(type = ifelse(type == "textInput" | type == "text", "t",
                                ifelse(type == "selectInput" | type == "select", "s",
                                       ifelse(type == "numericInput" | type == "numeric", "n",
                                              ifelse(type == "radioButtons" |  type == "radio", "r",
                                                     ifelse(type == "checkboxGroupInput" |  type == "checkbox", "c",
                                                            as.character(type))))))) %>%
    dplyr::mutate(across(everything(), .fns = as.character)) %>%
    dplyr::mutate(label = ifelse(priority == "r" & !is.null(requiredLab),
                                 paste0(label, " <span style=color:#CC3311>", requiredLab,
                                        "</span>"), as.character(label)))
  if ("trigger_id" %in% colnames(qs_init)) {
    qs <- qs_init

    # if (!all((qs_init$trigger_id %in% qs_init$id)|is.na(qs_init$trigger_id)))
    #  stop("Error: The trigger_id column must contain ONLY NA or a value from the 'id' column in 'questionFile'.")

  } else {
    qs <- qs_init %>% dplyr::mutate(trigger_id = NA)
  }
  qsl <- setNames(split(qs, seq(nrow(qs))), rownames(qs))
  ui <- shiny::tagList(shinyjs::useShinyjs(),
                       shinyjs::hidden(
                         htmltools::tags$div(id = ns("survey_div"),
                                             htmltools::tags$h1(id = ns("title"), title),
                                             htmltools::tags$h3(id = ns("subtitle"),  subtitle),
                                             htmltools::tags$br(), lapply(seq_along(qsl), function(i) {
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

                                               if (qsl[[i]]$type != "t" & qsl[[i]]$type != "r") {
                                                 opts <- c("", opts)
                                               }
                                               if (is.na(qsl[[i]]$trigger_id)) {
                                                 if (qsl[[i]]$type == "t") {
                                                   input <- shiny::textInput(inputId = ns(qsl[[i]]$id),
                                                                             label = HTML(qsl[[i]]$label),
                                                                             width = "100%")
                                                 } else if (qsl[[i]]$type == "s" | qsl[[i]]$type ==  "n") {
                                                   input <- shiny::selectInput(inputId = ns(qsl[[i]]$id),
                                                                               label = HTML(qsl[[i]]$label),
                                                                               choices = opts,
                                                                               selected = character())
                                                 } else if (qsl[[i]]$type == "r") {
                                                   input <- shiny::radioButtons(inputId = ns(qsl[[i]]$id),
                                                                                label = HTML(qsl[[i]]$label),
                                                                                width = "100%",
                                                                                choiceNames = opts,
                                                                                choiceValues = vals,
                                                                                selected = character())
                                                 } else {
                                                   input <- shiny::checkboxGroupInput(inputId = ns(qsl[[i]]$id),
                                                                                      label = HTML(qsl[[i]]$label),
                                                                                      width = "100%",
                                                                                      choiceNames = opts,
                                                                                      choiceValues = vals,
                                                                                      selected = character())
                                                 }
                                                 return(input)
                                               } else {
                                                 if (qsl[[i]]$type == "t") {
                                                   input <- shinyjs::hidden(shiny::textInput(inputId = ns(qsl[[i]]$id),
                                                                                             label = HTML(qsl[[i]]$label),
                                                                                             width = "100%"))
                                                 } else if (qsl[[i]]$type == "s" | qsl[[i]]$type ==  "n") {
                                                   input <- shinyjs::hidden(shiny::selectInput(inputId = ns(qsl[[i]]$id),
                                                                                               label = HTML(qsl[[i]]$label),
                                                                                               choices = opts,
                                                                                               selected = character()))
                                                 } else if (qsl[[i]]$type == "r") {
                                                   input <- shinyjs::hidden(shiny::radioButtons(inputId = ns(qsl[[i]]$id),
                                                                                                label = HTML(qsl[[i]]$label),
                                                                                                width = "100%",
                                                                                                choiceNames = opts,
                                                                                                choiceValues = vals,
                                                                                                selected = character()))
                                                 } else {
                                                   input <- shinyjs::hidden(shiny::checkboxGroupInput(inputId = ns(qsl[[i]]$id),
                                                                                                      label = HTML(qsl[[i]]$label),
                                                                                                      width = "100%",
                                                                                                      choiceNames = opts,
                                                                                                      choiceValues = vals,
                                                                                                      selected = character()))
                                                 }
                                                 return(input)
                                               }

                                               return(input)
                                             }),
                                             shinyjs::disabled(shiny::actionButton(ns("submit"), submitLab)),
                                             htmltools::tags$hr())))
  return(ui)
}
