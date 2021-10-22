#' Shiny Server function for documenting participant consent
#'
#' @param id The input ID associated with the consent module. Must match the ID of `consentUI()`.
#' @param trigger A reactive value indicating the event that should trigger the appearance of the consent form. May be an `input$...` value from outside the module wrapped in `reactive()`.
#' @param result A character value describing what should happen when the participant gives consent. Must be either "disable" or "hide".
#' @param delayResponse Integer. How long (in milliseconds) should the participant be forced to wait (i.e., to read the form text) before clicking "Agree" or "Disagree"? Defaults to no delay.
#' @param agreeLab Character. The text to be displayed on the button that the participant will click to give their consent. Defaults to "I agree".
#' @param disagreeLab If not `NULL` (the default), a button will be shown alongside the "agree" button, that the participant may click to decline their consent. The character string provided here will serve as the label for that button.
#' @param cons2rec Boolean. Should the participant be required to explicitly give their consent to be recorded? Default is `FALSE`.
#' @param cons2recRequire Boolean. If a separate consent to record is shown, should the participant be required to consent to voice recording before the can agree to the rest of the consent form and continue with the experiment? Defaults to `TRUE`.
#' @param cons2recLab Character. Label for the "Consent to Record" radio button pane. Defaults to a generic question about permission to record audio.
#' @param cons2recYes Character. Label for the radio button the participant will click to agree to audio recording. Defaults to a generic statement.
#' @param cons2recNo Character. Label for the radio button the participant will click to decline consent to audio recording. Defaults to a generic statement.
#'
#' @return A reactive value of type `integer` indicating whether the participant has given consent to the experiment and to record (=2), consent to everything not including recording (=1), or not consented to any portion of the experiment (=0).
#' @seealso Must be used with \code{\link{consentUI}}. If `cons2rec = TRUE`, consider using \code{\link{recorderUI}} and \code{\link{recorderServer}}.
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
consentServer <- function(id = "consent",
                          trigger,
                          result = c("disable", "hide"),
                          delayResponse = 1,
                          agreeLab = "Agree",
                          disagreeLab = NULL,
                          cons2rec = FALSE,
                          cons2recRequire = TRUE,
                          cons2recLab = "This experiment requires us to make audio recordings of your voice. Do you consent to having your voice recorded?",
                          cons2recYes = "I consent to my voice being recorded for this research.",
                          cons2recNo = "I do NOT grant my consent for my voice to be recorded."){

  if (result != "disable" & result != "hide")
    stop("Error: result parameter must either be 'disable' or 'hide'. ")

  shiny::moduleServer(
    id = id,

    function(input, output, session) {
      react <- shiny::reactiveValues(rec = cons2rec,
                                     rec_req = cons2recRequire)
      observeEvent(trigger(), {
        shinyjs::showElement("title")
        shinyjs::showElement("text")
        output$interface <- shiny::renderUI({
          ns <- session$ns
          shiny::tagList(
            if (isTRUE(cons2rec)) {
              shiny::radioButtons(ns("cons2rec_bttn"),
                                  label = cons2recLab,
                                  choiceNames = c(cons2recYes, cons2recNo),
                                  choiceValues = c("yes", "no"),
                                  selected = character(),
                                  width = "100%")
            },
            shinyjs::disabled(shiny::actionButton(ns("agree"), agreeLab)),
            if (!is.null(disagreeLab)) {
              shinyjs::disabled(shiny::actionButton(ns("disagree"), disagreeLab,
                                                    inline = TRUE))
            }
          )
        })

        shinyjs::delay(as.numeric(delayResponse),
                       shinyjs::enable("agree"))

        shinyjs::delay(as.numeric(delayResponse),
                       shinyjs::enable("disagree"))

        shiny::observeEvent(input$cons2rec_bttn, {
          if (isTRUE(react$rec_req) & input$cons2rec_bttn == "no") {
            shinyalert::shinyalert(text = "You must consent to voice recording to continue with this experiment. If you do not wish to participate, you may close your browser window; no information will be saved. Otherwise, click 'Return to Experiment' below to return to the consent form.",
                                   type = "info",
                                   confirmButtonText = "Return to Experiment",
                                   inputId = "return1",
                                   closeOnEsc = FALSE)
            shinyjs::disable("cons2rec_bttn")
            shinyjs::disable("agree")
            shinyjs::disable("disagree")
          }
        })

        shiny::observeEvent(input$agree, {
          if (isTRUE(react$rec_req) & is.null(input$cons2rec_bttn)) {
            shinyalert::shinyalert(text = "You must consent to voice recording to continue with this experiment. If you do not wish to participate, you may close your browser window; no information will be saved. Otherwise, click 'Return to Experiment' below to return to the consent form.",
                                   type = "info",
                                   confirmButtonText = "Return to Experiment",
                                   inputId = "return2",
                                   closeOnEsc = FALSE)
          }

          if (result == "disable") {
            shinyjs::disable("cons2rec_bttn")
            shinyjs::disable("agree")
            shinyjs::disable("disagree")
          } else {
            shinyjs::hide("title")
            shinyjs::hide("text")
            shinyjs::hide("cons2rec_bttn")
            shinyjs::hide("agree")
            shinyjs::hide("disagree")
            shinyjs::hide("end")
          }

        })

        shiny::observeEvent(input$disagree, {
          shinyalert::shinyalert(text = "This experiment requires your consent. If you do NOT wish to consent to the procedures outlined above, you may close this tab now. If you wish to consent to the procedures outlined above, click 'Return to Experiment' below.",
                                 type = "info",
                                 confirmButtonText = "Return to Experiment",
                                 inputId = "return3",
                                 closeOnEsc = FALSE)
          shinyjs::disable("cons2rec_bttn")
          shinyjs::disable("agree")
          shinyjs::disable("disagree")
        })

        shiny::observeEvent(input$return1|input$return2|input$return3, {
          shiny::updateRadioButtons(session,
                                    inputId = "cons2rec_bttn",
                                    label = cons2recLab,
                                    choiceNames = c(cons2recYes, cons2recNo),
                                    choiceValues = c("yes", "no"),
                                    selected = character())
          shinyjs::enable("agree")
          shinyjs::enable("disagree")
        })
      })

      retval <- shiny::eventReactive(input$agree, {
        if (isTRUE(react$rec) & input$cons2rec_bttn == "yes") {
          return(2)
        } else {
          return(1)
        }
      })

      return(retval)

    }
  )
  }
