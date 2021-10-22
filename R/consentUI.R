#' Shiny UI function for displaying a participant consent form
#'
#' @param id The input ID associated with the consent module. Must match the ID of `consentServer()`.
#' @param title The title of the consent form. Defaults to "Consent Form."
#' @param filename The name of the file containing the consent form text. This must be an HTML file stored in the app's `www` directory. If no filename is given, a generic consent text is used.
#'
#' @return A user interface that displays a custom consent form from a separate file, buttons for the participant to consent or decline, and (optionally) an opportunity for the participant to explicitly agree to audio recording.
#' @export
#' @seealso Must be used with \code{\link{consentServer}}. If `cons2rec = TRUE`, consider using \code{\link{recorderUI}} and \code{\link{recorderServer}}.
#' @family Consent form module
#' @examples
#' if (interactive()) {
#'   ui <- shiny::fluidPage(
#'     shiny::actionButton("ok", "OK"),
#'     consentUI(id = "cons"),
#'     shiny::actionButton("done", "DONE"),
#'     textOutput("result")
#'   )
#'   server <- function(input, output, session) {
#'     consent <- consentServer(id = "cons",
#'                              trigger = shiny::reactive(input$ok),
#'                              delayResponse = 3000,
#'                              disagreeLab = "Decline",
#'                              cons2rec = TRUE)
#'
#'     shiny::observeEvent(input$ok, {
#'       shinyjs::hide("ok")
#'     })
#'
#'     # Show the output value. In principle you'd probably want a command that tests whether the output of the consentServer is > 0.
#'     shiny::observeEvent(input$done, {
#'       output$result <- shiny::renderText({consent()})
#'     })
#'  }
#'   shiny::shinyApp(ui = ui, server = server)
#' }
#'
#'
consentUI <- function(id = "consent",
                      title = "Consent Form",
                      filename = NULL){
  ns <- shiny::NS(id)

  if (!is.null(filename)) {
    filename <- if (grepl("^www/",filename)) {
      filename
    } else if (grepl("^/", filename)){
      paste0("www", filename)
    } else {
      paste0("www/", filename)
    }

    if (!file.exists(filename))
      stop(paste0("File ", filename, " does not exist. Please place an HTML file containing your consent form text in your app's www directory."))
    if (!grepl("(\\.html$)",filename))
      stop(paste0("Consent form text must be in a .html file."))
  }

  ui <- shiny::tagList(
    shinyjs::useShinyjs(),
    shinyalert::useShinyalert(),
    shinyjs::hidden(htmltools::tags$h1(id = ns("title"), title)),
    shinyjs::hidden(htmltools::tags$span(id = ns("text"),
                         if(!is.null(filename)) {
                           htmltools::includeHTML(filename)
                         }else{
                           shiny::HTML("<p>Participation in this experiment is voluntary and requires your informed consent. Refusal to take part in the study will NOT result in penalty, and participants may withdraw from the study at any time without penalty. All responses are treated as confidential.</p><br><p><b>If you understand the statements above and freely consent to participate in the study, click the 'Agree' button to begin the experiment. Consent is required for participation; if you do not wish to participate, you may close the browser window now, and no data will be saved or used.</b></p>")
                         }
    )),
    shiny::uiOutput(ns("interface")),
    htmltools::tags$br(id = ns("end")))

}
