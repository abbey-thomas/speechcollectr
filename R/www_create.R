#' Create the 'www' Directory for a Shiny App
#'
#' @param from A character vector of filenames or a single directory name wrapped in c(). Leave `NULL` (default) if you only want to add one of the datasets included in the package.
#' @param is_dir Boolean. Is 'from' a directory?
#' @param path A character vector naming the path to the parent directory of 'www'. Defaults to working directory.
#' @param volumeCalibration Boolean. Should `speechcollectr::cal_noise()` for testing headphone volume be copied to 'www'?
#' @param HugginsPitchScreen Boolean. Should `speechcollectr::HugginsPitchData()` for Huggins Pitch Headphone Screen be copied to 'www'?
#' @param AntiphaseScreen Boolean. Should `speechcollectr::AntiphaseData()` for Antiphase Headphone Screen be copied to 'www'?
#'
#' @return Success messages will be printed in the console as each step towards creating the directory is completed.
#' @export
#'
#' @examples
#'
# TO DO:
# 1. Add recorder scripts.
# 2. Add game functions.
# 3. Add examples.
# 4. Change console output to give filenames.

www_create <- function(from = NULL, is_dir = FALSE, path = ".",
                       volumeCalibration = FALSE,
                       HugginsPitchScreen = FALSE,
                       AntiphaseScreen = FALSE){

  if (path == ".") {
    to <- paste0(getwd(), "/www")
  } else {
    if (isTRUE(grepl(".+?/$", path))) {
      to <- paste0(path, "www")
    } else {
      to <- paste0(path, "/www")
    }
  }

  if (!dir.exists("www")) {
    dir.create(path = to)
    cat("Success: Directory 'www' created!")
  }

  if (!is.null(from)) {

    if (isTRUE(is_dir)) {
      from <- list.files(path = from, full.names = TRUE)
    }

    cp <- file.copy(from = c(from), to = to, overwrite = TRUE)
    if (all(cp)) {
      cat("Success: All files in 'from' were added to 'www'!")
    }
  }

  if (isTRUE(volumeCalibration)) {
    vol_cal <- data("cal_noise")
    writeWave(vol_cal, "www/cal_noise.wave")
    cat("Success: Volume calibration noise WAV added to 'www'!")
  }

  if (isTRUE(HugginsPitchScreen)) {
    hp_data <- data("HugginsPitchData")
    for (i in 1:14) {
      tuneR::writeWave(hp_data[[i]], filename = paste0("www/", names(hp_data[i])))
    }
    cat("Success: Huggins-Pitch Headphone Screen data added to 'www'!")
  }

  if (isTRUE(AntiphaseScreen)) {
    hp_data <- data("AntiphaseData")
    for (i in 1:7) {
      tuneR::writeWave(ap_data[[i]], filename = paste0("www/", names(ap_data[i])))
    }
    cat("Success: Antiphase Headphone Screen data added to 'www'!")
  }

}
