#' A sample of white noise
#'
#' A 10 second sample of white noise useful for presenting to the user for headphone volume calibration in a shiny app.
#'
#' @format A Wave object (for more information, see `tuneR::readWave()` documentation) containing a 10 second sample of white noise sampled at 44.1 kHz.
#'
#' @source \url{https://github.com/ChaitLabUCL/HeadphoneCheck_Test}
#' @import tuneR
"cal_noise"

#' Basic Demographic Survey Questions.
#'
#' A data frame containing a correctly formatted basic demographic questionnaire, for use with the `speechcollectr::survey*()` functions.
#'
#' @format A data frame with 7 rows (corresponding to 7 survey questions) and 5 variables:
#' \describe{
#'   \item{id}{A unique input ID for each question}
#'   \item{label}{Labels (i.e., the questions) that will be displayed to participants}
#'   \item{priority}{Specifies whether an answer to a given question is required or not.}
#'   \item{type}{Type of shiny input function to use for each question.}
#'   \item{options}{Contains the list of options for all non-textInput questions.}
#'
#' }
"demographics"
