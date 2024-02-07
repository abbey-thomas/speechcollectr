#' Shiny Server function for documenting participant consent
#'
#' @param id The input ID associated with the consent module. Must match the ID of `consentUI()`.
#' @param trigger A reactive value indicating the event that should trigger the appearance of the consent form. May be an `input$...` value from outside the module wrapped in `reactive()`.
#' @param result A character value describing what should happen when the participant gives consent. Must be either "disable" or "hide".
#' @param delayResponse Integer. How long (in milliseconds) should the participant be forced to wait (i.e., to read the form text) before clicking "Agree" or "Disagree"? Defaults to no delay.
#' @param agreeLab Character. The text to be displayed on the button that the participant will click to give their consent. Defaults to "I agree".
#' @param agreeId Character. The inputId for the "agree" button. Defaults to "consent-yes".
#' @param disagreeLab If not `NULL` (the default), a button will be shown alongside the "agree" button, that the participant may click to decline their consent. The character string provided here will serve as the label for that button.
#' @param disagreeId If disagreeLab is not `NULL`, a character string representing the inputId for the disagree button. Defaults to "consent-no"
#' @param cons2rec Boolean. Should the participant be required to explicitly give their consent to be recorded? Default is `FALSE`.
#' @param cons2recRequire Boolean. If a separate consent to record is shown, should the participant be required to consent to voice recording before the can agree to the rest of the consent form and continue with the experiment? Defaults to `TRUE`.
#' @param cons2recLab Character. Label for the "Consent to Record" radio button pane. Defaults to a generic question about permission to record audio.
#' @param cons2recYes Character. Label for the radio button the participant will click to agree to audio recording. Defaults to a generic statement.
#' @param cons2recNo Character. Label for the radio button the participant will click to decline consent to audio recording. Defaults to a generic statement.
#'
#' @return A value of type `integer` indicating whether the participant has given consent to the experiment and to record (=2), consent to everything not including recording (=1), or not consented to any portion of the experiment (=0).
#' @seealso Must be used with \code{\link{consentUI}}. If `cons2rec = TRUE`, consider using \code{\link{recordUI}} and \code{\link{recordServer}}.
#' @export
#' @family Consent form module
#' @examples
#' if (interactive()) {
#'   ui <- shiny::fluidPage(
#'     shiny::actionButton("ok", "OK"),
#'     consentUI(id = "cons"),
#'     textOutput("result")
#'   )
#'   server <- function(input, output, session) {
#'     consent <- consentServer(id = "cons",
#'                              trigger = shiny::reactive(input$ok),
#'                              delayResponse = 3000,
#'                              agreeId = "agree",
#'                              disagreeLab = "Decline",
#'                              disagreeId = "disagree",
#'                              cons2rec = TRUE)
#'
#'     shiny::observeEvent(input$ok, {
#'       shinyjs::hide("ok")
#'     })
#'
#'     # Show the output value when the participant agrees.
#'     shiny::observeEvent(input$agree, {
#'       output$result <- shiny::renderText({consent$agree})
#'     })
#'  }
#'   shiny::shinyApp(ui = ui, server = server)
#' }
#'
consentServer <- function(id = "consent",
                          trigger = NULL,
                          result = c("disable", "hide"),
                          delayResponse = 1000,
                          agreeLab = "Agree",
                          agreeId = "consent-yes",
                          disagreeLab = NULL,
                          disagreeId = "consent-no",
                          cons2rec = FALSE,
                          cons2recRequire = TRUE,
                          cons2recLab = "This experiment requires us to make audio recordings of your voice.
                                         Do you consent to having your voice recorded?",
                          cons2recYes = "I consent to my voice being recorded for this research.",
                          cons2recNo = "I do NOT grant my consent for my voice to be recorded."){

  if (result != "disable" & result != "hide")
    stop("Error: result parameter must either be 'disable' or 'hide'. ")

  if (id == agreeId)
    stop("Error: 'id' and 'agreeId' must have different values!")

  session <- shiny::getDefaultReactiveDomain()

  react <- shiny::reactiveValues(rec = cons2rec,
                                 rec_req = cons2recRequire)
  returns <- shiny::reactiveValues()
  if (!is.null(trigger)) {
    shiny::observeEvent(trigger(), {
      shinyjs::showElement(paste0(id))

      session$output[[paste0(id, "-interface")]] <- shiny::renderUI({
        shiny::tagList(
          if (isTRUE(cons2rec)) {
            shiny::radioButtons(paste0(id, "-cons2rec_bttn"),
                                label = cons2recLab,
                                choiceNames = c(cons2recYes, cons2recNo),
                                choiceValues = c("yes", "no"),
                                selected = character(),
                                width = "100%")
          },
          shinyjs::disabled(shiny::actionButton(paste0(agreeId), agreeLab)),
          if (!is.null(disagreeLab)) {
            shinyjs::disabled(shiny::actionButton(paste0(disagreeId), disagreeLab,
                                                  inline = TRUE))
          }
        )
      })

      shinyjs::delay(delayResponse, shinyjs::click(paste(id, "-invis")))
    })
  } else {
    shiny::observe({
      shinyjs::showElement(id)

      session$output[[paste0(id, "-interface")]] <- shiny::renderUI({
        shiny::tagList(
          if (isTRUE(cons2rec)) {
            shiny::radioButtons(paste0(id, "-cons2rec_bttn"),
                                label = cons2recLab,
                                choiceNames = c(cons2recYes, cons2recNo),
                                choiceValues = c("yes", "no"),
                                selected = character(),
                                width = "100%")
          },
          shiny::actionButton(paste0(agreeId), agreeLab),
          if (!is.null(disagreeLab)) {
            shiny::actionButton(paste0(disagreeId), disagreeLab,
                                inline = TRUE)
          }
        )
      })
    })
  }

  shiny::observeEvent(session$input[[paste0(id, "-invis")]], {
    shinyjs::enable(paste0(agreeId))
    shinyjs::enable(paste(disagreeId))
  })

  shiny::observeEvent(session$input[[paste0(id, "-cons2rec_bttn")]], {
    if (isTRUE(react$rec_req) & paste0(id, "-cons2rec_bttn") == "no") {
      shiny::showModal(shiny::modalDialog(title = "Consent to Record Required!",
                                          shiny::tags$p("You must consent to voice recording to continue with this experiment. If you do not wish to participate, you may close your browser window; no information will be saved. Otherwise, click 'Return to Experiment' below to return to the consent form."),
                                          footer = shiny::actionButton(inputId = paste0(id, "-return"), label = "Return to Experiment")
      ))
      shinyjs::disable(paste0(agreeId))
      shinyjs::disable(paste0(disagreeId))
    }
  })

  shiny::observeEvent(session$input[[paste0(agreeId)]], {
    if (isTRUE(cons2rec)) {
      if (is.null(session$input[[paste0(id, "-cons2rec_bttn")]])) {
        if (isTRUE(react$rec_req)) {
          shiny::showModal(shiny::modalDialog(title = "Consent to Record Required!",
                                              shiny::tags$p("You must consent to voice recording to continue with this experiment. If you do not wish to participate, you may close your browser window; no information will be saved. Otherwise, click 'Return to Experiment' below to return to the consent form."),
                                              footer = shiny::actionButton(inputId = paste0(id, "-return"), label = "Return to Experiment")
          ))
        } else {
          returns$agree <- 1
        }
      } else {
        if (isTRUE(react$rec_req)) {
          returns$agree <- 1
        } else {
          returns$agree <- 2
        }
      }
    } else {
      returns$agree <- 1
    }

    if (result == "disable") {
      shinyjs::disable(paste0(id, "-cons2rec_bttn"))
      shinyjs::disable(paste0(agreeId))
      shinyjs::disable(paste0(disagreeId))
    } else {
      shinyjs::hide(paste0(id))
    }
  })

  shiny::observeEvent(session$input[[paste0(id, "-disagree")]], {
    returns$agree <- 0
    #shiny::showModal(shiny::modalDialog(title = "Consent Required",
    #                                    shiny::tags$p("This experiment requires your consent. If you do NOT wish to consent to the procedures outlined above, you may close this tab now. If you wish to consent to the procedures outlined above, click 'Return to Experiment' below."),
    #                                    footer = shiny::actionButton(inputId = paste0(id, "-return"), label = "Return to Experiment")
    #))

    shinyjs::disable(paste0(id, "-cons2rec_bttn"))
    shinyjs::disable(paste0(agreeId))
    shinyjs::disable(paste0(disagreeId))
  })

  shiny::observeEvent(session$input[[paste0(id, "-return")]], {
    shiny::removeModal()
    if (isTRUE(cons2rec)) {
      shiny::updateRadioButtons(session = session,
                                inputId = paste0(id, "-cons2rec_bttn"),
                                label = cons2recLab,
                                choiceNames = c(cons2recYes, cons2recNo),
                                choiceValues = c("yes", "no"),
                                selected = character())
    }
    shinyjs::showElement(paste0(id))
    shinyjs::enable(paste0(agreeId))
    shinyjs::enable(paste0(disagreeId))
  })

  return(returns)
}
