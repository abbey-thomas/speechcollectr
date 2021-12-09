#' Create the 'www' Directory for a Shiny App
#'
#' @description Many shiny apps for speech experiments require resources (like WAV files) in the www subdirectory. This command takes care of gathering datafiles and putting them in the appropriate directory for
#' @param from A character vector of filenames or a single directory name wrapped in c(). Leave `NULL` (default) if you only want to add one of the datasets included in the package.
#' @param is_dir Boolean. Is 'from' a directory?
#' @param path A character vector naming the path to the parent directory of 'www'. Defaults to working directory.
#' @param volumeCalibration Boolean. Should `cal_noise()` for testing headphone volume be copied to 'www'?
#' @param HugginsPitchScreen Boolean. Should `HugginsPitchData()` for Huggins Pitch Headphone Screen be copied to 'www'?
#' @param AntiphaseScreen Boolean. Should `AntiphaseData()` for Antiphase Headphone Screen be copied to 'www'?
#'
#' @return A www subdirectory in the current working directory. Success messages will be printed in the console as each step towards creating the directory is completed.
#' @export
#'
#' @examples
#'
# TO DO:

# 2. Add game functions.
# 3. Add examples.

wwwPrep <- function(from = NULL, is_dir = FALSE, path = ".",
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
    vol_cal <- cal_noise
    tuneR::writeWave(vol_cal, "www/cal_noise.wav")
    cat("Success: Volume calibration noise WAV added to 'www' as 'cal_noise.wav'.")
  }

  if (isTRUE(HugginsPitchScreen)) {
    hp_data <- HugginsPitchData
    for (i in 1:14) {
      tuneR::writeWave(hp_data[[i]], filename = paste0("www/", names(hp_data)[i], ".wav"))
    }
    cat("Success: Huggins-Pitch Headphone Screen data added to 'www'! All Huggins Pitch files (12) are prefixed with 'hp_'.")
  }

  if (isTRUE(AntiphaseScreen)) {
    ap_data <- AntiphaseData

    for (i in 1:7) {
      tuneR::writeWave(ap_data[[i]], filename = paste0("www/", names(ap_data)[i], ".wav"))
    }
    cat("Success: Antiphase Headphone Screen data added to 'www'! All antiphase screen files (6) are prefixed with 'ap_'.")
  }

}
