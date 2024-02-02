#' Use the recording scripts to record audio
#'
#' @return Adds the javascript files for audio recording to the ui object. Only needed when using \code{\link{startRec}} and \code{\link{stopRec}}, NOT when using the recording module!
#' @export
#'
#' @examples
#' if (interactive()){
#'  library(shiny)
#'  library(shinyjs)
#'
#'  # Build UI
#'  ui <- fluidPage(
#'
#'    ## Get background javascript ready
#'    useShinyjs(),
#'    tags$head(
#'      useRecorder()
#'    ),
#'
#'    ## Setup page layout
#'    div(id = "trialDiv",
#'        style = "text-align:center;",
#'
#'        hidden(div(id = "textDiv",
#'                   h4("Please record yourself reading this sentence aloud:"),
#'                   h2(textOutput(outputId = "read_this")))),
#'
#'        ## Create the buttons for controlling the recording
#'        actionButton(inputId = "start",
#'                     label = "start"),
#'
#'        ### Hide the stop button until user clicks start
#'        hidden(actionButton(inputId = "stop",
#'                            label = "stop"))
#'    )
#'  )
#'
#'  # The Server function
#'  server <- function(input, output, session) {
#'    ## Create an object that will count trials
#'    rvs <- reactiveValues(trial_n = 0)
#'
#'    ## When the start button is clicked
#'    observeEvent(input$start, {
#'
#'      ### Increase counter by one
#'      rvs$trial_n <- rvs$trial_n + 1
#'
#'      ### Start the recording
#'      startRec()
#'
#'      ### Disable the start button
#'      disable("start")
#'
#'      ### Show the stop button
#'      delay(500, showElement("stop"))
#'    })
#'
#'    observeEvent(input[["rec-ready"]], {
#'      showElement("textDiv")
#'      output$read_this <- renderText({paste0("This is recording ",
#'                                             rvs$trial_n, ".")})
#'    })
#'
#'    ## When the user clicks stop
#'    observeEvent(input$stop, {
#'
#'      ### Stop recording
#'      stopRec(filename = paste0("rec", rvs$trial_n, ".wav"))
#'
#'      ### Enable the start button
#'      enable("start")
#'
#'      ### Hide the stop button
#'      hide("stop")
#'      hide("textDiv")
#'    })
#'  }
#'
#'  # Run the application
#'  shinyApp(ui = ui, server = server)
#' }
#'
useRecorder <- function() {
  ui <- shiny::tagList(
    shiny::includeScript(system.file("recorder/rec_backend.js",
                                     package = "speechcollectr")),
    shiny::includeScript(system.file("recorder/rec_frontend.js",
                                     package = "speechcollectr"))
  )

  return(ui)
}
