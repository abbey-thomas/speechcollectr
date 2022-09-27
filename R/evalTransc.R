#' Check whether a Transcription Matches the Correct Value
#'
#' @param filename An RDS file output from `transcribeServer()` OR an RDS file containing a single character string representing the entered transcription.
#' @param text A character string to test against the correct transcription. Not necessary if `filename` is supplied.
#' @param correct A character string of the correct transcription to which `text` or `filename` will be compared.
#' @param alphaOnly Boolean. Should the transcription be compared only in alphabetic characters entered, ignoring any spaces, punctuation, or digits? Defaults to TRUE.
#' @param matchCase Boolean. Should the comparison be case-sensitive? Defaults to FALSE.
#'
#' @return A Boolean value indicating whether the transcription in `text` or `filename` matches the value given in `correct` (=1) or not (=0).
#' @details This function is likely not very useful on its own, except for post-processing of transcriptions entered with `transcribeUI()` and `transcribeServer()`. This function serves as a base for its interactive version \code{\link{evalTranscServer}}, for giving feedback to a participant based on a transcription they've entered.
#' @export
#'
#' @examples
#' # Using the filename argument
#' transc <- "this is a sample transcription"
#' saveRDS(transc, "foo.rds")
#'
#' is.correct <- evalTransc(filename = "foo.rds",
#'                          correct = "This is a sample transcription.")
#' print(is.correct)
#'
#' # Using the text argument instead
#' # Play with the values matchCase and alphaOnly to observe changes in the returned value.
#' is.correct <- evalTransc(text = "this is a sample transcription",
#'                          correct = "This is a sample transcription.")
#' print(is.correct)
#'
evalTransc <- function(filename = NULL,
                       text,
                       correct,
                       alphaOnly = TRUE,
                       matchCase = FALSE) {
  if (!is.null(filename)){
    text <- readRDS(filename)

    if ("entered" %in% names(text)) {
      text <- text["entered"]
    }
  }

  if (!isTRUE(matchCase)) {
    text <- tolower(text)
    correct <- tolower(correct)
  }

  if (isTRUE(alphaOnly)) {
    text <- gsub("[^[:alpha:]]", "", text)
    correct <- gsub("[^[:alpha:]]", "", correct)
  }

  if (text == correct) {
    return(1)
  } else {
    return(0)
  }
}
