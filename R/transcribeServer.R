#' Make the Transcription Interface Interactive
#'
#' @param id The module id. Must be the same as \code{\link{transcribeUI}}.
#' @param If not `NULL` (the default), a reactive expression returning the event that should trigger the appearance of the interface.
#' @param audioFile The file path to a file relative to the applications www directory. (The file must be in the www directory, but the path should not include the "www/" prefix).
#' @param outFile If not NULL (the default), a file path with the extension .rds, which will store the name of the audio file, the transcription entered, and the number of times the participant played the audio.
#' @param allowPause Boolean. Should the participant be given a button to pause the audio during the transcription? Defaults to FALSE.
#' @param n_play Integer. How many times can a participant play the audio file before clicking "submit"? Defaults to 1.
#' @param result One of "hide" or "clear". Should the interface be hidden or simply cleared (restored to its original state) when a participant clicks submit?
#' @param instructions Character. Instructions that will appear above the text entry area. Defaults to generic instructions for transcription.
#' @param n_lines Integer. The number of lines the text input area should occupy. Defaults to 1. Could be set higher for longer transcriptions, so the entire text entered remains visible to the participant.
#' @param width Character. Defaults to "100%", defines how much of the horizontal space the transcription interface will occupy.
#' @param submitLab The label on the submit button. Defaults to "SUBMIT".
#'
#' @return Returns a set of reactive values including the name of the audio file, the text the participant entered, and the number of times they clicked play.
#' @export
#'
#' @examples
#' data("samp_wav")
#' tuneR::writeWave(samp_wav, "sample.wav")
#' wwwPrep(from = "sample.wav")
#'
#' if (interactive()) {
#'   library(shiny)
#'   library(speechcollectr)
#'   library(shinyjs)
#'
#'   ui <- fluidPage(
#'     actionButton("begin", "Begin"),
#'     transcribeUI(id = "transcribe"),
#'     h5("Click 'Begin' each time you want to transcribe. The recording will not change from trial to trial."),
#'     h5("To demonstrate the transcription evaluation procedure, we'll use a different 'correct transcritiption' for each trial."),
#'     h5("The correct answers for the first 4 trials are: 'a', 'ab', 'abc', 'abcd' (in that order).")
#'   )
#'
#'   server <- function(input, output, session) {
#'     rvs <- reactiveValues(n = 1, attempt = 1)
#'     answers <- c("a", "ab", "abc", "abcd")
#'
#'     observeEvent(input$begin, {
#'       disable("begin")
#'       rvs$out <- transcribeServer(id="transcribe",
#'                                   audioFile = "sample.wav",
#'                                   n_play = 4,
#'                                   outFile = paste0("sample", rvs$n, ".rds"),
#'                                   result = "hide")
#'     })
#'
#'     observeEvent(input[["transcribe-submit"]], {
#'       delay(500,
#'             correct <- evalTranscServer(filename = paste0("sample", rvs$n, ".rds"),
#'                                         correct = answers[rvs$n],
#'                                         attempts = 2,
#'                                         counter = rvs$attempt,
#'                                         passInputId = "pass",
#'                                         warnInputId = "warn",
#'                                         failInputId = "fail"))
#'     })
#'   }
#'
#'   observeEvent(input$pass, {
#'     rvs$n <- rvs$n + 1
#'     rvs$attempt <- 1
#'     enable("begin")
#'   })
#'
#'   observeEvent(input$warn, {
#'     rvs$attempt <- rvs$attempt + 1
#'     enable("begin")
#'   })
#'
#'   shinyApp(ui = ui, server = server)
#' }

transcribeServer <- function(id = "transcribe",
                             trigger = NULL,
                             audioFile,
                             outFile,
                             allowPause = FALSE,
                             n_play = NULL,
                             result = c("clear", "hide"),
                             instructions = "Play the audio file. Then, enter what you heard in the box below. Click the submit button when you are finished.",
                             n_lines = 1,
                             width = "100%",
                             submitLab = "SUBMIT"){

  session <- shiny::getDefaultReactiveDomain()
  transcribe_rvs <- shiny::reactiveValues(play = 0, n = 1)

  trigger <- shiny::reactive(1)
  shiny::observeEvent(trigger(), {
    shinyjs::showElement(id)
    if (isTRUE(allowPause)) {
      session$output[[paste0(id, "-ui")]] <- shiny::renderUI({
        shiny::tagList(
          playBttn(inputId = paste0(id, "-play"),
                   audioId = paste0(id, "-audio"),
                   src = paste0(audioFile),
                   label=" Play"),
          pauseBttn(inputId = paste0(id, "-pause"),
                    audioId = paste0(id, "-audio"),
                    label = " Pause"),
          shiny::tags$br(),
          shiny::tags$br(),

          shinyjs::disabled(shiny::textAreaInput(inputId = paste0(id, "-text", transcribe_rvs$n),
                                                 label = instructions,
                                                 width = width,
                                                 rows = n_lines)),
          shiny::tags$br(),
          shinyjs::disabled(shiny::actionButton(inputId = paste0(id, "-submit"),
                                                label = paste0(submitLab)))
        )
      })
    } else {
      session$output[[paste0(id, "-ui")]] <- shiny::renderUI({
        shiny::tagList(
          playBttn(inputId = paste0(id, "-play"),
                   audioId = paste0(id, "-audio"),
                   src = paste0(audioFile),
                   label=" Play"),
          shiny::tags$br(),
          shiny::tags$br(),

          shinyjs::disabled(shiny::textAreaInput(inputId = paste0(id, "-text", transcribe_rvs$n),
                                                 label = instructions,
                                                 width = width,
                                                 rows = n_lines)),
          shiny::tags$br(),
          shinyjs::disabled(shiny::actionButton(inputId = paste0(id, "-submit"),
                                                label = paste0(submitLab)))
        )
      })
    }
  })

  shiny::observeEvent(session$input[[paste0(id, "-play")]], {
    transcribe_rvs$play <- transcribe_rvs$play + 1
    shinyjs::disable(paste0(id, "-play"))
    shinyjs::enable(paste0(id, "-text", transcribe_rvs$n))

    if (!is.null(n_play)) {
      if (transcribe_rvs$play < n_play) {
        shinyjs::delay(1000, shinyjs::enable(paste0(id, "-play")))
      }
    } else {
      shinyjs::delay(1000, shinyjs::enable(paste0(id, "-play")))
    }
  })

  shiny::observeEvent(session$input[[paste0(id, "-text", transcribe_rvs$n)]], {

    shiny::req(session$input[[paste0(id, "-text", transcribe_rvs$n)]])
    if (length(session$input[[paste0(id, "-text", transcribe_rvs$n)]]) > 0) {
      shinyjs::enable(paste0(id, "-submit"))
      out <- c(audio = paste0(audioFile),
               entered = session$input[[paste0(id, "-text", transcribe_rvs$n)]],
               n_played = transcribe_rvs$play)
      if (!is.null(outFile)) {
        if (grepl("rds$", outFile)) {
          saveRDS(out, outFile)
        }
        else if (grepl("csv$", outFile)) {
          write.csv(as.data.frame(t(out)), outFile, row.names = FALSE)
        }
      }
    }

    shiny::observeEvent(session$input[[paste0(id, "-submit")]], {
      shinyjs::disable(paste0(id, "-submit"))
      shinyjs::enable(paste0(id, "-play"))
      shinyjs::disable(paste0(id, "-text", transcribe_rvs$n))

      if (result == "hide") {
        shinyjs::hide(paste0(id))
      }
      transcribe_rvs$play <- 0
      transcribe_rvs$n <- transcribe_rvs$n+1
    })
  })

  retval <- shiny::eventReactive(session$input[[paste0(id, "-submit")]], {
      list(audio = paste0(audioFile),
           entered = session$input[[paste0(id, "-text", transcribe_rvs$n)]],
           n_played = transcribe_rvs$play)
  })

  return(retval)
}
