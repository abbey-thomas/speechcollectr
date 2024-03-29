% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/playBttn.R
\name{playBttn}
\alias{playBttn}
\title{Load Audio and Create a Play Button to Start Playback}
\usage{
playBttn(
  inputId,
  src,
  audioId,
  label = "",
  inline = FALSE,
  icon = "play",
  fill = "#228833",
  textCol = "white"
)
}
\arguments{
\item{inputId}{The input ID of the play button.}

\item{src}{The filename for the audio file to play. Must be in the "www/" folder for the app.}

\item{audioId}{The input ID to associate with the audio file that will be loaded.}

\item{label}{An optional text label to add to the button. Defaults to blank. NOTE: If icon is \code{NULL}, this variable should be given a value explicitly.}

\item{inline}{Boolean. If FALSE (the default), the oause button will be on its own line in the UI (i.e., wrapped in a \code{div} tag). If TRUE, the button will be on a line with the preceeding shiny UI object (i.e., wrapped in a \code{span} tag).}

\item{icon}{An optional \code{shiny::icon()} to add to the button. Defaults to the font-awesome "play" icon. NOTE: If no text label is present, icon cannot be null.}

\item{fill}{Background color for the play button. Must be a valid color name or hexidecimal code.}

\item{textCol}{Text or icon color for the label on the play button.}
}
\value{
An audio file with the ID \code{audioId} will be added to the list of input values. On the server side of a shiny app, this function returns an integer of class "shinyActionButtonValue", which means it can be used in all the ways an ordinary shiny action button can (e.g., you can add functions to be called when the button is pressed with \code{shiny::observeEvent()} and \code{shiny::eventReactive}).
}
\description{
Load Audio and Create a Play Button to Start Playback
}
\examples{
# First use wwwPrep() to get a wav file in the correct directory.
wwwPrep(volumeCalibration = TRUE)

## Only run examples in interactive R sessions
if (interactive()) {
ui <- fluidPage(
  "Play a sound: ",
   playBttn(inputId = "play", src = "cal_noise.wav",
        audioId = "cal_noise"),
   disabled(pauseBttn(inputId = "pause", audioId = "cal_noise"))
 )

# disable the play button and enable the pause button after play button is clicked.
server <- function(input, output, session) {
 observeEvent(input$play, {
   disable("play")
   enable("pause")
 })
}
shinyApp(ui = ui, server = server)
}

## Use inline = TRUE to display the button on the same line as the preceding UI element.
if (interactive()) {
ui <- fluidPage(
  "Play a sound: ",
   playBttn(inputId = "play", src = "cal_noise.wav",
        audioId = "cal_noise", inline = TRUE),
   pauseBttn(inputId = "pause", audioId = "cal_noise", inline = TRUE)
 )

server <- function(input, output, session) {}

shinyApp(ui = ui, server = server)
}

## Use parameter 'label =' to add a text label to the button.
## Use icon = NULL to remove the icon.
if (interactive()) {
ui <- fluidPage(
   playBttn(inputId = "play", label = "Play noise", src = "cal_noise.wav",
        audioId = "cal_noise", inline = TRUE, icon = NULL),
   pauseBttn(inputId = "pause", audioId = "cal_noise", inline = TRUE)
 )

server <- function(input, output, session) {}

shinyApp(ui = ui, server = server)
}
}
