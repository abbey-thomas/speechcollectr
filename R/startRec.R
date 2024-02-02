#' Start Recording User Audio in a Shiny App
#'
#' @description Requests access to the user's microphone and begins audio recording if permission is granted. For use only inside a call to `observeEvent()` or `bindEvent()` in a Shiny application's server code.
#' @return Adds the value "record-ready" to the session's input object. The event `input[["rec-ready"]]` event can be used to trigger other actions (like the appearance of some text for the participant to read).
#' @export
#' @seealso Should be used with \code{\link{stopRec}}
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
startRec <- function() {
  session <- shiny::getDefaultReactiveDomain()
  el <- shiny::reactiveVal(1)
  session$sendCustomMessage("startRec", el)
}
