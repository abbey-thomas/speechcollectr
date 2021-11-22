#' Generate a unique ID for a participant
#'
#' @param file Path to an RDS or CSV file. Should be in the www directory in a Shiny app. If the specified file does not exist, it will be created.
#' @param digits Integer. How many digits long should each ID number be? Defaults to 4.
#' @param random Boolean. Should the unique ID be random (=TRUE), or should it be equal to the ID of the previous participant + 1 (=FALSE)?
#' @param reactive Boolean. Should the value returned be a Shiny reactive value?
#'
#' @return A reactive or non-reactive unique ID number, with the number of digits specified by `digits`, appended to the list denoted by `file`. `file` is overwritten to include the new unique ID each time the function is called.
#' @note If you create this value inside `observeEvent()` (not recommended, see example below), be sure to inizialize the value in your Shiny server outside of the observer.
#'
#' @examples
#' library(shiny)
#' library(shinyjs)
#'
#' ui <- fluidPage(
#'   fluidRow(
#'     column(width = 8, offset = 2,
#'            br(),
#'            actionButton("assignID",
#'                         "Click to get your unique ID number."),
#'            textOutput("pin"))
#'
#'   )
#' )
#'
#' server <- function(input, output, session) {
#'
#'   #Putting the pinGen() here, outside the observeEvent expression, generates one and only one ID # per session... The safer option!
#'   pin <- pinGen(file = "pins.rds",
#'                 reactive = TRUE)
#'
#'   observeEvent(input$assignID, {
#'     # If you put pinGen() here, be sure to disable or hide the button you use to trigger pin generation! You do not want this to be generated multiple times for a participant.
#'     shinyjs::disable("assignID")
#'     output$pin <- renderText({paste0("Your ID # is ", pin())})
#'   })
#' }
#'
#' shinyApp(ui = ui, server = server)
pinGen <- function(file = "www/pinlist.rds",
                   digits = 4,
                   random = FALSE,
                   reactive = TRUE) {
  if (!grepl("csv$", file) & !grepl("rds$", file))
    stop("Argument 'file' must be of the format '.csv' or '.rds'.")

  if (file.exists(file)) {
    if (grepl("csv$", file)) {
      pins <- read.csv(file)
    } else {
      pins <- readRDS(file)
    }
  } else {
    pins <- c(10^(digits-1))
  }

  if (isTRUE(random)) {
    min <- 10^(digits-1)
    max <- 10^(digits)-1
    pin <- sample(c(min:max), 1)
    while(pin %in% pins) {
      pin <- sample(c(min:max), 1)
    }
  } else {
    pin <- max(pins) + 1
  }

  all <- c(pins, pin)
  if (grepl("csv$", file)) {
    write.csv(data.frame(pin = numeric(all)), file, row.names = FALSE)
  } else {
    saveRDS(all, file)
  }

  if (isTRUE(reactive)) {
    return(shiny::reactive(pin))
  } else {
    return(pin)
  }
}
