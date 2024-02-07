#' Create UI to Record User Audio
#'
#' @description The user-interface side of the Shiny module to record user audio in Speech Production experiments. A participant chooses when to begin recording and when to stop by clicking the buttons created in this module. Requires the server-side function \code{\link{recordServer}}.
#' @param id The input ID associated with the record module. Must be the same as the id of `recordServer()`.
#' @param startText Character. The text label for the "start recording" button. Defaults to "RECORD".
#' @param startTextCol A valid color name in R or a hexidecimal color code denoting the color of the start button text. Defaults to "white".
#' @param startFillCol A valid color name in R or a hexidecimal color code denoting the color of the start button text. Defaults to the "green" hex code from Paul Tol's colorblind-safe \emph{bright} qualitative color scheme.
#' @param stopText Character. The text label for the "stop recording" button. Defaults to "STOP".
#' @param stopTextCol A valid color name in R or a hexidecimal color code denoting the color of the start button text. Defaults to "black".
#' @param stopFillCol A valid color name in R or a hexidecimal color code denoting the color of the start button text. Defaults to Defaults to the "red" hex code from Paul Tol's colorblind-safe \emph{bright} qualitative color scheme.
#' @param startInline Boolean. Should the 'start recording' button be on the same line as the preceding UI element?
#' @param stopInline Boolean. Should the 'stop recording' button be placed on the same line as the 'start recording' button?
#' @param submitText Text to appear on the button that will hide the recording interface when the user finishes recording. Defaults to "SUBMIT RECORDING".
#' @param align One of 'left', 'center', or 'right'. Should the elements in this UI be left-, center-, or right-aligned?
#'
#' @return A user interface containing buttons to control recording of participant audio. Also provides an optional placeholder for a written stimulus, and a submit button that, when clicked, will hide the recording interface.
#' @note The "start" and "stop" buttons from this module can be accessed in the server code with the "id" of the module as follows: `input[["id-start"]]` or `input[["id-stop"]]`
#' @export
#' @family Audio recording module
#' @seealso Must be used with \code{\link{recordServer}}. Note that \code{\link{useRecorder}} is not required with the recording module. Paul Tol's colorblind-safe palettes (the source of the default button colors) can be found at \url{https://personal.sron.nl/~pault/#sec:qualitative}.
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   library(shinyjs)
#'
#'   # Build UI
#'   ui <- fluidPage(
#'
#'     ## Get background javascript ready
#'     useShinyjs(),
#'
#'     ## Setup page layout
#'     div(id = "trialDiv",
#'         style = "text-align:center;",
#'         actionButton("next_trial", "NEXT"),
#'
#'         ### Initialize the recording interface invisibly
#'         hidden(recordUI(id = "rec_module"))
#'     )
#'   )
#'
#'   # The Server function
#'   server <- function(input, output, session) {
#'     ## Create an object that will count trials
#'     rvs <- reactiveValues(trial_n = 0)
#'
#'     ## When the participant clicks "NEXT"...
#'     observeEvent(input$next_trial, {
#'       ### Increase the trial number
#'       rvs$trial_n <- rvs$trial_n + 1
#'
#'       ### Show the recording interface
#'       showElement("rec_module")
#'
#'       ### Hide the next button
#'       hide("next_trial")
#'     })
#'
#'     ## Note that the call to recordServer() is at the top level of our app's server function
#'     ## And the returned filename and text to be read are wrapped in reactive()
#'     ## (since they need to be reactive).
#'     recordServer(id = "rec_module",
#'                  attempts = 3, overwrite = TRUE,
#'                  outFile = reactive(paste0("www/rec", rvs$trial_n, ".wav")),
#'                  writtenStim = reactive(paste0("This is recording ", rvs$trial_n, ".")))
#'
#'     ## As with all speechcollectr modules, the submit button's Id
#'     ## can be accessed with the module id + `-submit`
#'     ## Here when the user clicks submit inside the recording module...
#'     observeEvent(input[["rec_module-submit"]], {
#'
#'       ## Make the next button reappear
#'       showElement("next_trial")
#'     })
#'
#'   }
#'   # Run the application
#'   shinyApp(ui = ui, server = server)
#' }
#'
#' @importFrom grDevices col2rgb
#'
recordUI <- function(id = "record",
                     startText = "RECORD",
                     startTextCol = "white",
                     startFillCol = "#228833",
                     stopText = "STOP",
                     stopTextCol = "black",
                     stopFillCol = "#EE6677",
                     startInline = FALSE,
                     stopInline = TRUE,
                     submitText = "SUBMIT RECORDING",
                     align = "center") {
  if (tryCatch(is.matrix(col2rgb(startFillCol)),
               error = function(e) FALSE) == FALSE) {
    stop("Error: startFillCol argument requires a valid color name or hexadecimal code.")
  }

  if (tryCatch(is.matrix(col2rgb(startTextCol)),
               error = function(e) FALSE) == FALSE) {
    stop("Error: startTextCol argument requires a valid color name or hexadecimal code.")
  }

  if (tryCatch(is.matrix(col2rgb(stopFillCol)),
               error = function(e) FALSE) == FALSE) {
    stop("Error: stopFillCol argument requires a valid color name or hexadecimal code.")
  }

  if (tryCatch(is.matrix(col2rgb(stopTextCol)),
               error = function(e) FALSE) == FALSE) {
    stop("Error: stopTextCol argument requires a valid color name or hexadecimal code.")
  }

  ui <- shiny::tags$div(id = paste0(id),
                        useRecorder(),
                        shinyjs::useShinyjs(),
                        style = paste0("text-align:", align,";"),
                        shiny::tags$div(style = paste0("height:100px;"),
                                        shinyjs::hidden(shiny::tags$h3(id = paste0(id, "-stimDiv"),
                                                                       shiny::textOutput(paste0(id, "-stim"))))
                        ),
                        shiny::actionButton(paste0(id, "-start"),
                                            label = startText,
                                            style = paste0("color: ", col2hex(startTextCol),
                                                           "; background-color: ",
                                                           col2hex(startFillCol)),
                                            inline = startInline),

                        shinyjs::disabled(
                          shiny::actionButton(paste0(id, "-stop"),
                                              label = stopText,
                                              style = paste0("color: ", col2hex(stopTextCol),
                                                             "; background-color: ",
                                                             col2hex(stopFillCol)),
                                              inline = stopInline)),
                        shiny::tags$br(),
                        shiny::tags$br(),
                        shinyjs::disabled(
                          shiny::actionButton(paste0(id, "-submit"),
                                              label = paste0(submitText))
                        )
  )
  return(ui)
}
