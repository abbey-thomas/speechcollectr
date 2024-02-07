#' Stop Recording User Audio and Save WAV File
#'
#' @description For use only inside a call to `observeEvent()` or `bindEvent()` in a Shiny application's server function.
#' @param filename Required. A character vector giving the file name of the WAV file that will be saved.
#' @param finishedId Character. ID of the input object created when the recording is saved.
#'
#' @return Adds a to the application's input object (with the inputId given by finishedId) after saving a WAV file containing the audio data recorded from the user's microphone, stored in the location given in the filename argument. The value of the object with the inputId given by "finishedId" is equal to the name of the returned file.
#' @export
#' @seealso Should be preceded by \code{\link{startRec}} in the application's server code.
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
#'                            label = "stop")),
#'        textOutput(outputId = "file_saved")
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
#'      startRec(readyId = "ready")
#'
#'      ### Disable the start button
#'      disable("start")
#'
#'      ### Show the stop button
#'      delay(500, showElement("stop"))
#'    })
#'
#'    observeEvent(input$ready, {
#'      showElement("textDiv")
#'      output$read_this <- renderText({paste0("This is recording ",
#'                                             rvs$trial_n, ".")})
#'    })
#'
#'    ## When the user clicks stop
#'    observeEvent(input$stop, {
#'
#'      ### Stop recording
#'      stopRec(filename = paste0("rec", rvs$trial_n, ".wav"),
#'              finishedId = "done")
#'
#'      ### Enable the start button
#'      enable("start")
#'
#'      ### Hide the stop button
#'      hide("stop")
#'      hide("textDiv")
#'    })
#'
#'    observeEvent(input$done, {
#'      output$file_saved <- renderText({paste0("The file '", input$done, "' was saved.")})
#'    })
#'  }
#'
#'  # Run the application
#'  shinyApp(ui = ui, server = server)
#' }
#'
stopRec <- function(filename) {
  session <- shiny::getDefaultReactiveDomain()

  randId <- paste0(sample(c(sample(letters, 10),
                            sample(c(1:9), 10, replace = TRUE)), 20),
                   collapse = "")

  el2 <- paste0("rec-audio-", randId)
  session$sendCustomMessage("stopRec", el2)

  observeEvent(session$input[[paste0(el2)]], {
    audio <- session$input[[paste0(el2)]]
    audioOut <- gsub("data:audio/wav;base64,", "", audio)
    audioOut <- gsub(" ", "+", audioOut)
    audioOut <- RCurl::base64Decode(audioOut, mode = "raw")
    inFile <- file(paste0(filename), "wb")
    writeBin(audioOut, inFile)
    close(inFile)
  })

  observeEvent(session$input[[paste0(el2)]], priority = -1, {
    el3 <- list(finishedId = paste(finishedId),
                outFile = paste(filename))
    session$sendCustomMessage("recDone", el3)
  })
}
