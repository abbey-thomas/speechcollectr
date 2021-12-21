#' Check a recording for clipping
#'
#' @param wave Required. Either a `tuneR::Wave` object or a valid file path to an existing wav file.
#' @param return Either "df", "logical", or "proportion". Should the function return a dataframe containing a list of clipped frames, a logical value indicating whether more samples than the amount indicated by `max_clipped` were clipped, or the amount of samples clipped as a proportion of the signal?
#' @param threshold The amplitude threshold above which samples should be considered clipped. Defaults to 32112.64 (maximum for 16-bit PCM).
#' @param max_clipped The proportion of samples we can allow to be clipped and still return `FALSE` if return value is logical. Defaults to 0.01 (i.e. 1% of samples).
#'
#' @return A data.frame containing a list of clipped samples or a logical value indicating whether more samples than the amount indicated by `max_clipped` were clipped
#'
#' @examples
#' data("samp_wav")
#' fs <- findSpeech(samp_wav)
#' #find the beginning of the utterance in seconds (to compare with manual measurements in Praat, for example).
#' fs$begin_s/samp_wav@samp.rate
#' #now the end...
#' fs$end_s/samp_wav@samp.rate

clipCheck <- function(wave,
                      return = c("df", "logical", "proportion"),
                      threshold = 32112.64,
                      max_clipped = 0.01){
  if (class(wave)[1] != "Wave") {
    wave <- tuneR::readWave(wave)
  }

  df <- data.frame(amp = as.numeric(wave@left)) %>%
    dplyr::mutate(clip = ifelse(abs(amp >= threshold), TRUE, FALSE)) %>%
    dplyr::filter(clip == TRUE)

  if (return == "df") {
    return(df)
  } else if (return == "logical"){
    if (length(df$clip) > (length(wave@left)*max_clipped)) {
      clipping <- TRUE
    } else {
      clipping <- FALSE
    }
    return(clipping)
  } else {
    clipping <- round(length(df$clip)/length(wave@left), digits = 2)
    return(clipping)
  }
}
