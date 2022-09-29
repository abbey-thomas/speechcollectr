#' Server Function for the Headphone Test Module
#'
#' @description The server portion of the Headphone Test module. Make your headphoneTestUI() do things!
#'
#' @param id The Id of this module. Must match the Id of `headphoneTestUI()`. Defaults to "headphone_test".
#' @param trigger Optional. A reactive expression indicating the event that should trigger the appearance of the consent form. Must be wrapped in `reactive()`.
#' @param type Whether to use the Huggins Pitch headphone screen (see Milne et al., 2020) or the Antiphase headphone screen (Woods et al., 2017). Must be one of "huggins" or "antiphase" and must match the value given in `headphoneTestUI()`.
#' @param n_trials Integer. How many trials should be presented to each participant? Defaults to 6. Must match the n_trials argument of `headphoneTestUI()`
#' @param threshold Integer. How many trials must a participant get correct to pass the screen?
#' @param n_attempts Integer. How many attempts to pass the screen should a participant get? Defaults to 2.
#' @param warn_msg An optional character vector. Customize the text of the warning message that appears when a participant fails the screen but is given another opportunity to pass.
#' @param fail_msg An optional character vector. Customize the text of the message shown to participants when they fail the headphone screen on their final attempt.
#' @param success_msg An optional character vector. Customize the text of the "success" message when the participant passes the headphone screen.
#'
#' @return A reactive value, status, that has a value of 0 (failed/did not complete headphone screen) or 1 (passed headphone screen).
#' @family headphone screen module
#' @seealso \code{\link{wwwPrep}}, \code{\link{headphoneTestUI}}
#' @export
#' @references
#' Milne, A.E., Bianco, R., Poole, K.C., Zhao, S., Oxenham, A.J., Billing, A.J., & Chait, M. 2021. An online headphone screening test based on dichotic pitch.\emph{Behavior Research Methods 53}, 1551-1562.\cr
#' Woods, K.J.P., Siegel, M.H., Traer, J., & McDermott, J.H. 2017. Headphone screening to facilitate web-based auditory experiments. \emph{Attention, Perception, & Psychophysics 79}, 2064-2072.
#'
#' @examples
#' ## First use wwwPrep() to get the data for the type of headphone screen you want.
#' ## NOTE: Do NOT put this command in your UI!
#' # Run it once before testing your app to create the www folder.
#' wwwPrep(HugginsPitchScreen = TRUE)
#'
#' # Now build the app.
#' if (interactive()) {
#'  ui <- fluidPage(
#'         headphoneTestUI(id = "headphone_test", type = "huggins"),
#'         textOutput("done")
#'      )
#'
#'  server <- function(input, output, session) {
#'    # Give the reactive value output a name, so it can be checked and accessed later.
#'            phones <- headphoneTestServer(id = "headphone_test", type = "huggins",
#'                                          n_trials = 6, threshold = 4, n_attempts = 2)
#'            observe({
#'              if (phones() == 1) {
#'                  output$done <- renderText("Headphone test successfully completed!")
#'                  }
#'               })
#'   }
#' shinyApp(ui = ui, server = server)
#' }
#' @importFrom magrittr %>%
#' @importFrom rlang .data
headphoneTestServer <- function(id = "headphone_test",
                                trigger = NULL,
                                type = c("huggins", "antiphase"),
                                n_trials = 6, threshold,
                                n_attempts = 2,
                                warn_msg = "default",
                                fail_msg = "default",
                                success_msg = "default") {
  if (type != "huggins" & type != "antiphase") stop("'type' must be equal to either 'huggins' or 'antiphase'.")
  if (threshold > n_trials) stop("'threshold' cannot be greater than 'n_trials!'")

  shiny::moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      trial <- shiny::reactiveVal(0)
      mistakes <- shiny::reactiveVal(0)
      attempt <- shiny::reactiveVal(1)
      status <- shiny::reactiveVal(NULL)

      if (type == "huggins") {
        files <- data.frame(filename =
                              list.files(path = "www/hps_audio",
                                         pattern = "hp_\\d")) %>%
          tidyr::extract(col = .data$filename, into = "answer",
                         regex = ".+?_(\\d)\\.wav", remove = FALSE) %>%
          dplyr::mutate(answer = paste0("answer",
                                        as.character(.data$answer)))
      } else {
        files <- data.frame(filename =
                              list.files(path = "www/hps_audio",
                                         pattern = "ap_\\d")) %>%
          tidyr::extract(col = .data$filename, into = "answer",
                         regex = ".+?_(\\d)\\.wav", remove = FALSE) %>%
          dplyr::mutate(answer = paste0("answer",
                                        as.character(.data$answer)))
      }

      samp <- sample(nrow(files), n_trials)
      files <- files[samp,]
      print(files)

      if (is.null(trigger)) {
        shiny::observe({
          shinyjs::showElement("adjust")
        })
      } else {
        shiny::observeEvent(trigger(), {
          shinyjs::showElement("adjust")
        })
      }

      shiny::observeEvent(input$adjust_play, {
        shinyjs::enable("adjust_done")
        shinyjs::disable("adjust_play")
        shinyjs::enable("adjust_pause")
      })

      shiny::observeEvent(input$adjust_pause, {
        shinyjs::disable("adjust_pause")
        shinyjs::enable("adjust_play")
      })

      shiny::observeEvent(input$adjust_done, {
        shinyjs::enable("adjust_play")
        shinyjs::disable("adjust_pause")
        shinyjs::hide("adjust")
        shinyjs::showElement("instr")

        if (type == "antiphase") {
          shinyjs::delay(2500, shinyjs::enable("ready"))
        }
      })

      shiny::observeEvent(input$test, {
        shinyjs::disable("test")

        shinyjs::delay(5000, shinyjs::enable("test"))
        shinyjs::delay(5500, shinyjs::enable("ready"))
      })

      shiny::observeEvent(input$ready, {
        shinyjs::hide("instr")
        shinyjs::showElement("progress")
        shinyjs::showElement("screen")
      })

      shiny::observeEvent(input$screenplay, {
        shinyjs::disable("screenplay")
        shiny::insertUI(paste0("#", ns("screenplay")),
                        where = "afterEnd",
                        ui = htmltools::tags$audio(src = files$filename[trial() + 1],
                                                   type = "audio/wav",
                                                   autoplay = NA, controls = NA,
                                                   style="display:none;"))
        shinyjs::delay(4100, shinyjs::enable("choices"))
      })

      shiny::observeEvent(input$choices, {
        shinyjs::enable("submit")
      })

      shiny::observeEvent(input$submit, {
        shinyjs::disable("submit")
        if (input$choices != as.character(files$answer[trial() + 1])) {
          mistakes(mistakes() + 1)
        }

        if (mistakes() > (n_trials-threshold)) {
          if (attempt() < n_attempts) {
            shinyalert::shinyalert(type = "warning",
                                   text = if (warn_msg == "default") {paste0("Unfortunately, you did not pass the headphone screen. Please make sure you are in a quiet environment and have BOTH headphones on properly. To continue with the experiment, click 'Try Again'. You will have a total of ", n_attempts, " chances to pass the headphone screen. If you do not wish to continue, you may close your browser window now.")
                                   } else {as.character(warn_msg)},
                                   confirmButtonText = "Try Again",
                                   inputId = "warn",
                                   closeOnEsc = FALSE)

            trial(0)
            mistakes(0)
            attempt(attempt() + 1)
            status(0)

            shinyjs::hide("screen")
            shinyjs::showElement("adjust")

          } else {
            shinyjs::hide("screen")
            shinyalert::shinyalert(type = "error",
                                   text = if (fail_msg == "default") {"Unfortunately you did not pass the headphone screen. You have used all available attempts. Fully functioning headphones are required for this experiment. Thank you for your time!"
                                   } else {as.character(fail_msg)},
                                   confirmButtonText = "OKAY",
                                   inputId = "fail")
            status(0)
          }
        }
        if (trial() < (n_trials-1)) {
          trial(trial() + 1)
          shinyWidgets::updateProgressBar(session = session,
                                          id = session$ns("progress"),
                                          value = trial(), total = n_trials,
                                          range_value = c(1:n_trials))

          shinyWidgets::updateRadioGroupButtons(session, "choices",
                                                label = if (type == "huggins") {"Select the sound containing the hidden tone..."
                                                } else {"Select the quietest sound..."},
                                                choices = list("Sound 1" = "answer1",
                                                               "Sound 2" = "answer2",
                                                               "Sound 3" = "answer3"),
                                                selected = character(0),
                                                disabled = TRUE)

          shinyjs::delay(500, shinyjs::enable("screenplay"))
        } else {
          shinyjs::hide("screen")
          shinyalert::shinyalert(type = "success",
                                 title = "Success!",
                                 text = if (success_msg == "default") {"You passed the headphone screen."
                                 } else {as.character(success_msg)},
                                 timer = 3000)
          status(1)
        }

      })

      return(shiny::reactive(status()))
    }
  )
}
