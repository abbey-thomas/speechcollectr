consentUI <- function(id = "consent",
                      title = "Consent Form",
                      filename = "consent.html",
                      delayResponse = FALSE,
                      agreeLab = "I Agree",
                      disagreeLab = NULL,
                      cons2rec = FALSE,
                      cons2recRequire = TRUE,
                      cons2recLab = "This experiment requires us to make audio recordings of your voice. Do you consent to having your voice recorded?",
                      cons2recYes = "I consent to my voice being recorded for this research.",
                      cons2recNo = "I do NOT grant my consent for my voice to be recorded."
                      ){

  ns <- shiny::NS(id)

  filename <- if (grepl("^www/",filename)) {
    filename
  } else if (grepl("^/", filename)){
    paste0("www", filename)
  } else {
    paste0("www/", filename)
  }

  if (!file.exists(filename))
    stop(paste0("File ", filename, " does not exist. Please place an HTML or txt file containing your consent form text in your app's www directory."))
  if (!grepl("(\\.txt$)|(\\.html$)",filename))
    stop(paste0("Consent form text must be in a .html file or a .txt file."))

  ui <- shiny::tagList(
    shinyjs::useShinyjs(),
    htmltools::tags$h1(ns("title"), title),

    htmltools::tags$span(ns("text"),
      if (grepl("html$", filename)) {
        htmltools::includeHTML(filename)
      } else {
        htmltools::tags$pre(htmltools::includeText(filename))
      }
    ),

    if (isTRUE(cons2rec)) {
      shiny::radioButtons(ns("cons2rec"),
                          label = cons2recLab,
                          choices = c(cons2recYes = "yes",
                                      cons2recNo = "no"),
                          selected = character(),
                          width = "100%")
    },

    if (isTRUE(cons2recRequire)|isTRUE(delayResponse)) {
      shinyjs::disabled(shiny::actionButton(ns("agree"), agreeLab))
    } else {
      shiny::actionButton(ns("agree"), agreeLab)
    },

    if (!is.null(disagreeLab)) {
      if (isTRUE(delayResponse)){
        shinyjs::disabled(shiny::actionButton("disagree", disagreeLab,
                                              inline = TRUE))
      } else {
        shiny::actionButton("disagree", disagreeLab, inline = TRUE)
      }
    },
    htmltools::tags$br(id = ns("end"))
  )

}
