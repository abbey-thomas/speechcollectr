#' UI function for the Headphone Test Module
#'
#' @description Create the user interface for a headphone screen using Huggins pitch or Antiphase phenomena.
#'
#' @param id The Id of this module. Must match the Id of \code{\link{headphoneTestServer}}. Defaults to "headphone_test".
#' @param type Whether to use the Huggins Pitch headphone screen (see Milne et al., 2020) or the Antiphase headphone screen (Woods et al., 2017). Must be one of "huggins" or "antiphase" and must match the value given in `headphoneTestServer()`.
#' @param n_trials Integer. How many trials should be presented to each participant? Defaults to 6. Must match the n_trials argument of \code{\link{headphoneTestServer}}
#'
#' @return Creates the user interface for a headphone screen. Must be used with `headphoneTestServer()` in the server function of the app.
#' @export
#'
#' @family headphone screen module
#' @seealso \code{\link{www_create}}, \code{\link{headphoneTestUI}}
#' @references
#' Milne, A.E., Bianco, R., Poole, K.C., Zhao, S., Oxenham, A.J., Billing, A.J., & Chait, M. 2020. An online headphone screening test based on dichotic pitch.\emph{Behavior Research Methods} 53, 1551-1562 (2021).
#' Woods, K.J.P., Siegel, M.H., Traer, J., & McDermott, J.H. 2017. Headphone screening to facilitate web-based auditory experiments. \emph{Attention, Perception, & Psychophysics} 79, 2064-2072 (2017).
#'
#' @examples
#' ## First use www_create() to get the data for the type of headphone screen you want.
#' ## NOTE: Do NOT put this command in your UI!
#' ## Run it once before testing your app to create the www folder.
#' www_create(HugginsPitchScreen = TRUE)
#'
#' # Now build the app.
#' if (interactive()) {
#'  ui <- fluidPage(
#'         headphoneTestUI(id = "headphone_test", type = "huggins")
#'      )
#'
#'  server <- function(input, output, session) {
#'                            headphoneTestServer(id = "headphone_test", type = "huggins",
#'                                                 n_trials = 6, threshold = 4, n_attempts = 2)
#'   }
#' shinyApp(ui = ui, server = server)
#' }
headphoneTestUI <- function(id = "headphone_test",
                            type = c("huggins", "antiphase"),
                            n_trials = 6) {

  ns <- shiny::NS(id)

  if (type != "huggins" & type != "antiphase") stop("'type' must be equal to either 'huggins' or 'antiphase'.")

  if (!dir.exists("www")) stop("You must create a 'www' subdirectory with the necessary sound files. Use function www_create() before running your ShinyApp for the first time.")
  if (type == "huggins") {
    if (!file.exists("www/hp_practice.wav")) stop("Use www_create(HugginsPitchScreen = TRUE) before running your ShinyApp to get the sound files needed for this headphone screen.")
  }

  if (!file.exists("www/cal_noise.wav")) stop("Calibration noise file does not exist. Use www_create() to download the sound files before running the headphone screen. Run '?www_create()' for more information.")

  shiny::tagList(
    shinyalert::useShinyalert(),
    shinyjs::useShinyjs(),
    htmltools::tags$table(id = ns("adjust"), style = "border: 1px solid #c5c5c5;",
                          htmltools::tags$tr(htmltools::tags$th(style = "padding: 15px; border: 1px solid #c5c5c5;","Adjust your Sound Volume...")),
                          htmltools::tags$tr(htmltools::tags$th(style = "padding: 15px; text-align: center;","Please put on your headphones.")),
                          htmltools::tags$tr(htmltools::tags$td(style = "padding: 15px;","If you do not have headphones, you may use earbuds.")),
                          htmltools::tags$tr(htmltools::tags$td(style = "padding: 15px;","Click 'PLAY' below to listen to a sample of white noise. Adjust your volume to a comfortable level.")),
                          htmltools::tags$tr(htmltools::tags$td(style = "padding: 15px; text-align: center;",
                                                                playBttn(inputId = ns("adjust_play"),
                                                                         src = "cal_noise.wav", audioId = "volume_test",
                                                                         label = "PLAY", icon = NULL),
                                                                shinyjs::disabled(pauseBttn(inputId = ns("adjust_pause"), audioId = "volume_test", inline = TRUE, label = "PAUSE", icon = NULL)))),
                          htmltools::tags$tr(htmltools::tags$td(style = "padding: 15px; border: 1px solid #c5c5c5; text-align: right;",
                                                                shinyjs::disabled(pauseBttn(inputId = ns("adjust_done"), audioId = "volume_test", label = "I have finished adjusting the volume.", icon = NULL))))),

    shinyjs::hidden(htmltools::tags$table(id = ns("instr"), style = "border: 1px solid #c5c5c5;",
                                          htmltools::tags$tr(htmltools::tags$th(style = "padding: 15px; border: 1px solid #c5c5c5;","Headphones Check")),
                                          htmltools:: tags$tr(htmltools::tags$th(style = "padding: 15px; text-align: center;","Let's make sure your headphones are functioning correctly for the experiment.")),
                                          htmltools::tags$tr(htmltools::tags$td(style = "padding: 15px;","Each of the next six pages will have a button you can click to play a recording. You can only play each sound once, so don't press the button until you are ready.")),
                                          htmltools::tags$tr(htmltools::tags$td(style = "padding: 15px;",
                                                                                if (type == "huggins") {"Each recording contains three sounds. One of the sounds contains a faint tone. Your task is to find the noise with the hidden tone."
                                                                                } else {"Each recording contains three sounds. Your job is to decide which noise is the QUIETEST."})),
                                          htmltools::tags$tr(htmltools::tags$td(style = "padding: 15px;",
                                                                                if (type == "huggins") {"After you have heard all three sounds and know which one contains the tone, you will see three buttons. Click the button that corresponds to the sound you thought was contained the tone."}
                                                                                else {"After you have heard all three sounds and know which one is quietest, you will see three buttons. Click the button that corresponds to the sound you thought was QUIETEST."})),
                                          if (type == "huggins") {htmltools::tags$tr(htmltools::tags$td(style = "padding: 15px;","Here is an example. You may play this sound as many times as you need. The sound that contains the tone is the second one, so the correct answer would be 'Sound 2'."))},
                                          if (type == "huggins") {htmltools::tags$tr(htmltools::tags$td(style = "padding: 20px; text-align: center;",
                                                                                                        playBttn(inputId = ns("test"),
                                                                                                                 src = "hp_practice.wav", audioId ="hp_practice",
                                                                                                                 label = "PLAY SOUNDS", icon = NULL)))},
                                          htmltools::tags$tr(htmltools::tags$th(style = "padding: 15px; text-align: center;","YOU WILL ONLY BE ALLOWED TO PLAY EACH SOUND ONCE, so make sure you are ready (your headphones are on, your environment is quiet, and the recording has your full attention).")),
                                          htmltools::tags$tr(htmltools::tags$th(style = "padding: 15px; text-align: center;","Are you ready?")),
                                          htmltools::tags$tr(htmltools::tags$td(style = "padding: 15px; border: 1px solid #c5c5c5; text-align: right;",
                                                                                shinyjs::disabled(shiny::actionButton(inputId = ns("ready"), "Begin Headphone Check")))))),


    shinyjs::hidden(htmltools::tags$table(id = ns("screen"), style = "border: 1px solid #c5c5c5;",
                                          htmltools::tags$tr(htmltools::tags$th(style = "padding: 15px; border: 1px solid #c5c5c5;","Headphones Check"),
                                                             htmltools::tags$td(style = "padding: 5px; border: 1px solid #c5c5c5;", htmltools::tags$div(shinyWidgets::progressBar(id = ns("progress"),
                                                                                                                                                    value = 0, total = n_trials,
                                                                                                                                                    range_value = c(1:n_trials))))),
                                          htmltools::tags$tr(htmltools::tags$th(colspan = 2, style = "padding: 15px; text-align: center;","Remember, you can only play each recording once. Please listen carefully.")),
                                          htmltools::tags$tr(htmltools::tags$td(colspan = 2, style = "padding: 15px; text-align: center;",
                                                                                if (type == "huggins") {"Which sound contains the hidden tone? Is it Sound 1, Sound 2, or Sound 3?"}
                                                                                else {"Which sound is quietest? Is it Sound 1, Sound 2, or Sound 3?"})),
                                          htmltools::tags$tr(htmltools::tags$td(style = "padding: 20px; text-align: center;",
                                                                                shiny::actionButton(ns("screenplay"), "PLAY SOUNDS")),
                                                             htmltools::tags$td(style = "padding: 15px; text-align: center;",
                                                                                shinyjs::disabled(shinyWidgets::radioGroupButtons(ns("choices"),
                                                                                                                                  label = if (type == "huggins") {"Select the sound containing the hidden tone..."
                                                                                                                                  } else {"Select the quietest sound..."},
                                                                                                                                  choices = list("Sound 1" = "answer1",
                                                                                                                                                 "Sound 2" = "answer2",
                                                                                                                                                 "Sound 3" = "answer3"),
                                                                                                                                  selected = character(0))))),
                                          htmltools::tags$tr(htmltools::tags$td(colspan = 2, style = "padding: 15px; border: 1px solid #c5c5c5; text-align: center;",
                                                                                shinyjs::disabled(shiny::actionButton(inputId = ns("submit"), "Submit Answer & Continue"))))))
  )

}
