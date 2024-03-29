% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/checkUI.R
\name{checkUI}
\alias{checkUI}
\title{A User Interface for quick T/F questions about a participant's qualifications or environment}
\usage{
checkUI(
  id = "check",
  title = "",
  type = c("participant", "environment"),
  instructions = "default",
  align = "center"
)
}
\arguments{
\item{id}{The module ID. Must be the same as the ID of \code{checkServer()}.}

\item{title}{Character. Usually, the title of the experiment. But can be anything else you want to be printed in large friendly letters at the top of the page.}

\item{type}{Either "participant" or "environment", to specify whether you want to ask questions about a participant's qualifications (i.e., do they meet inclusion criteria) or their acoustic environment (i.e., is it quiet enough for speech research).}

\item{instructions}{What should the participant do when the TRUE/FALSE question appears?}

\item{align}{One of 'left', 'center', or 'right'. Should the elements in this UI be left-, center-, or right-aligned?}
}
\value{
User interface elements for TRUE/FALSE questions, displayed one at a time.
}
\description{
A User Interface for quick T/F questions about a participant's qualifications or environment
}
\examples{
 # First get some sample questions for your participant.
data("qualifications")
write.csv(qualifications, "qualifications.csv", row.names = FALSE)

# Now ask the questions!
if (interactive()) {
  shinyApp(
    ui = fluidPage(
      fluidRow(
        column(width = 8, offset = 2,
               actionButton("btn", "Click me"),
               checkUI(id = "example", title = "Speech Experiment",
                       type = "participant"),
               textOutput("confirmation"))
      )

    ),
    server = function(input, output, session) {
      answer <- checkServer(id = "example",
                            trigger = reactive(input$btn),
                            questionFile = "qualifications.csv",
                            outFile = NULL,
                            returnVals = c("eighteen"))
      observeEvent(input$btn, {
        shinyjs::hide("btn")
      })
      observe({
        if (isTruthy(answer$eighteen))
          output$confirmation <- renderText("This participant is an adult.")
      })
    }
  )
}
}
