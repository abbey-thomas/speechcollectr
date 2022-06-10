#' Check Questions before Running a Survey in a Shiny App
#'
#' @param questionFile character. The name of the CSV file containing the id, label, type, priority, and options for each question. See Details below for more information.
#' @param notListedLab character. For non-textInput questions, the name of the choice that will allow your participants to enter their own value for all questions that include this string in the 'options' column.
#'
#' @return
#' A data frame that includes the same columns and rows as the questionFile, and gives feedback on each cell of the original data frame in the corresponding cells of the output. Optionally, if running in an interactive session, this function will also return a demo Shiny App, if the user replies 'y' (yes) when asked if they would like to preview the questions.
#'
#' @details The input for this function must be a CSV file containing all and only the columns described below. Each row in the CSV file corresponds to a survey question that will be displayed to participants with the `speechcollectr::survey*()` functions. If any of the columns described below are missing, or if your initial question file contains extra columns, this function will return ONLY an error message in the console. If this occurs, please go back, correct column names (they are case sensitive), save your question file, and then rerun `surveyPrep()`. If this function does not return a data frame in which all cells contain 'No errors', the `speechcollectr::survey*()` functions will not produce a Shiny survey form as expected.
#' \itemize{
#' \item{"id:"}{Gives a unique input ID for each question. Values MUST be unique for each row and follow the same rules as other Shiny input IDs (no spaces).}
#' \item{"label:"}{Contains the labels (i.e., the questions) that will be displayed to participants, as labels in the Shiny `*input()` functions.}
#' \item{"priority:"}{Specifies whether an item is required or not. There are two accepted options (required and optional), denoted by the following values (values separated by pipe (|) are treated as equivalent): 'required'|'req'|'r' and 'optional'|'opt'|'o'.}
#' \item{"type:"} {Tells Shiny which `*Input()` function to use for each question. Acceptable types are any of the following (equivalent options separated by '|'): 'textInput'|'text'|'t', 'selectInput'|'select'|'s', 'numericInput'|'numeric'|'n', 'checkboxGroupInput'|'checkbox'|'c',  and 'radioButtons'|'radio'|'r'.}
#' \item{"options:"} {Contains the list of options for all non-textInput questions. All options in the list should be separated by commas, and no enclosing parantheses are needed. If you wish to include an option for participants to enter a value not listed among these options, place the value for the `notListedLab` as the last option in the row. This will tell R to insert a textInput UI when the participant selects the not listed option. The value they enter will be accessible using the input ID associated with this row in the input CSV file. If you do not wish users to be able to specify a value when the select not listed, simply leave the `notListedLab` argument as `NULL`. If a question is of type numericInput, this column must contain two values, the minimum and the maximum numeric options, separated by a comma. If a question is of type textInput, the value in this column should be '' (an empty vector) or `NA`. If all questions are of type textInput, this column may be omitted.}
#' }
#'
#' @note The data frame produced by this function is only for informational purposes. Do NOT use this function in a shiny app to present to participants. To present a survey to participants use `surveyUI()` and `surveyServer()`. If any errors are found in your input question file, please return open the input question file in a text editor, correct any mistakes, and rerun this function until all cells in the output say 'No errors'.
#'
#' @examples
#' data("demographics")
#' write.csv(demographics, "www/demographics.csv", row.names = FALSE)
#' feedback <- surveyPrep(questionFile = "www/demographics.csv",
#'                        notListedLab = "Not listed:")
#'
surveyPrep <- function(questionFile = NULL,
                           notListedLab = NULL) {
  qs <- read.csv(file = questionFile)

  if (!"id" %in% colnames(qs))
    stop("Your question file must contain a column called 'id' that gives a unique input ID for each question.")
  if (!"label" %in% colnames(qs))
    stop("Your question file must contain a column called 'label' that contains the text for each question.")
  if (!"priority" %in% colnames(qs))
    stop("Your question file must contain a column called 'priority', and all values in this column must be either 'required'|'req'|'r'  or 'optional'|'opt'|'o' (equivalent options separated by '|').")
  if (!"type" %in% colnames(qs))
    stop("Your question file must contain a column called 'type' that tells shiny what function to use for each question. Values must be one of (equivalent options separated by '|'): 'textInput'|'text'|'t', 'selectInput'|'select'|'s', 'numericInput'|'numeric'|'n', or 'radioButtons'|'radio'|'r'.")
  extra <- qs %>% dplyr::select(-(c(id, label, priority, type, options)))
  if (ncol(extra) != 0)
    stop("You seem to have extra columns in your dataset. Your CSV file must contain ALL and ONLY the following columns: id, label, type, required, and options (options may be omitted if ALL your questions are of the type textInput). Any additional columns will be ignored.")


 qs <- qs %>%
    dplyr::mutate(priority = ifelse(priority == "required"|priority == "req", "r",
                                     ifelse(priority == "optional"|priority == "opt", "o",
                                            as.character(priority)))) %>%
    dplyr::mutate(type = ifelse(type == "textInput"|type == "text", "t",
                                ifelse(type == "selectInput"|type == "select", "s",
                                       ifelse(type == "numericInput"|type == "numeric", "n",
                                              ifelse(type == "radioButtons"|type == "radio", "r",
                                                     ifelse(type == "checkboxGroupInput"|type == "checkbox", "c",
                                                     as.character(type))))))) %>%
   dplyr::mutate(type_exist = ifelse(type=="t"|type=="s"|type=="n"|type=="r"|type=="c",
                                     "y", "n"))

 rowID <- list()
 id <- list()
 priority <- list()
 label <- list()
 type <- list()
 options <- list()

 for (i in 1:nrow(qs)) {
   rowID[[i]] <- qs$id[i]

   idck <- qs %>% filter(id == qs$id[i])
   if (nrow(idck) > 1) {
     id[[i]] <- paste0("ID '", qs$id[i], "' is used more than once.")
   } else {id[[i]] <- "No errors"}

   if (qs$priority[i] == "o" | qs$priority[i] == "r"){
     priority[[i]] <- "No errors"
   } else {
     priority[[i]] <- "All values in this column must be either 'required'|'req'|'r'  or 'optional'|'opt'|'o' (equivalent options separated by '|')."
   }

   if (qs$type_exist[i]=="y"){
     type[[i]] <- "No errors"
   } else {
     type[[i]] <- "Invalid question type. Each value in the type column must be one of: 'textInput'|'text'|'t', 'selectInput'|'select'|'s', 'numericInput'|'numeric'|'n', 'checkboxGroupInput'|'checkbox'|'c', or 'radioButtons'|'radio'|'r' (equivalent options separated by '|')."
   }

   if (is.na(qs$label[i])|length(qs$label[i])==0) {
     label[[i]] <- "Note: This question will not have a label."
   } else {
     label[[i]] <- "No errors"
   }

   if (qs$type[i] != "t"){
     opts <- gsub(", ", ",", qs$options[i])
     opts <- c(unlist(strsplit(opts, ",")))

     if (qs$type[i] == "n"){
     if (length(opts) != 2|opts[1] >= opts[2]) {
       options[[i]] <- "Options for a numeric input question must be exactly two numbers, the min and max, separated by a comma (,), and the min must be less than the max."
     } else {options[[i]] <- "No errors."}
     } else {
       if (length(opts) == 0) {
         options[[i]] <- "You must include the choices your participants may select from."
       } else {
         options[[i]] <- "No errors."
       }
     }
   } else {
     options[[i]] <- "No errors."
   }

   if (grepl(notListedLab, qs$options[i])) {
     options[[i]] <- paste0(options[[i]], " Note: The value specified as `notListedLab` was included the options for this question. A UI output will be created. If you do NOT wish users to be able to enter a value after clicking the not listed option, set the argument of `notListedLab` to NULL in the `surveyUI()` function.")
   }
 }

 feedback <- cbind(rowID, id, priority, label, type, options)

 if (interactive()) {
   ans <- readline("Would you like to preview these questions in a demo shiny app? (y|n)")
   if (substr(ans,1,1) == "y") {
     qsl <- setNames(split(qs, seq(nrow(qs))), rownames(qs))

     ui <- shiny::fluidPage(
       shiny::fluidRow(
         shiny::column(width = 8,
                       offset = 2,
                       htmltools::tags$br(),
                       lapply(seq_along(qsl), function(i) {
                         if (qsl[[i]]$type != "t") {
                           opts <- gsub(", ", ",", qsl[[i]]$options)
                           opts <- c(unlist(strsplit(opts, ",")))
                         }

                         if (qsl[[i]]$type == "n") {
                           opts <- as.numeric(opts)
                           opts <- c(opts[1]:opts[2])
                         }

                         if (qsl[[i]]$type == "r") {
                           vals <- gsub("[^[:alnum:]]", "", opts)
                         }

                         if (qsl[[i]]$type == "t") {
                           input <- shiny::textInput(inputId = qsl[[i]]$id,
                                                     label = qsl[[i]]$label,
                                                     width = '100%')
                         } else if (qsl[[i]]$type == "s"|qsl[[i]]$type == "n") {
                           input <- shiny::selectInput(inputId = qsl[[i]]$id,
                                                       label = qsl[[i]]$label,
                                                       choices = opts,
                                                       selected = character())
                         } else if (qsl[[i]]$type == "r") {
                           input <- shiny::radioButtons(inputId = qsl[[i]]$id,
                                                        label = qsl[[i]]$label,
                                                        width = '100%',
                                                        choiceNames = opts,
                                                        choiceValues = vals,
                                                        selected = character())
                         } else {
                           input <- shiny::checkboxGroupInput(inputId = qsl[[i]]$id,
                                                              label = qsl[[i]]$label,
                                                              width = '100%',
                                                              choiceNames = opts,
                                                              choiceValues = vals,
                                                              selected = character())
                         }
                          return(input)
                       }),
                       shiny::actionButton("close", "CLOSE")
         )
       )
     )

     server <- function(input, output, session) {
       lapply(seq_along(qsl), function(i){
         shiny::observeEvent(input[[paste0(qsl[[i]]$id)]], {
           if (as.character(input[[paste0(qsl[[i]]$id)]]) == notListedLab) {
             shiny::insertUI(paste0("#", qsl[[i]]$id),
                             where = "afterEnd",
                             ui = shiny::textInput(
                               inputId = paste0(qsl[[i]]$id, "_nl"),
                               label = "Type your answer to add it to the list of choices above:"))
           }
         })

         shiny::observeEvent(input[[paste0(qsl[[i]]$id, "_nl")]], {
           tmp <- input[[paste0(qsl[[i]]$id, "_nl")]]
           shiny::updateSelectInput(session, paste0(qsl[[i]]$id),
                             choices = tmp,
                             selected = tmp)
         })
       })
       shiny::observeEvent(input$close, {shiny::stopApp()})
       session$onSessionEnded(function() {shiny::stopApp()})
     }
     app <- shiny::shinyApp(ui = ui, server = server)
     shiny::runApp(app)

   }
 } else {
   print("Review of questions is complete.")
 }

 return(feedback)

}


