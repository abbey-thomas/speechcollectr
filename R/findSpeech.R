#' Detect a single utterance in a recording
#'
#' @param wave Required. Either a `tuneR::Wave` object or a valid file path to an existing wav file.
#' @param minAmp The minimum envelope value required for a signal to be considered speech. Defaults to 500. See `Details` for more information.
#' @param maxPause The maximum duration of a pause in the utterance. If a segment of non-speech longer than this value is found, it will be considered the end of the utterance.
#' @param beginPad How many samples should be included before the beginning of the detected signal to ensure we capture the whole utterance? See `Details` for more information.
#' @param endPad How many samples should be included after the end of the detected signal to ensure we capture the whole utterance?
#'
#' @return A list with 2 numeric values: begin_s = the sample in the original recording that marks the start of the speech signal; and end_s = the sample in the original recording that marks the end of the speech signal.
#' @details If you find the function is excluded too much speech, try lowering the minAmp value or increasing the begin and end padding values. If the function is including too much leading and trailing noise, try increasing the minAmp value or decreasing the begin and end pad values.
#' @export
#' @examples
#' data("samp_wav")
#' fs <- findSpeech(samp_wav)
#' #find the beginning of the utterance in seconds (to compare with manual measurements in Praat, for example).
#' fs$begin_s/samp_wav@samp.rate
#' #now the end...
#' fs$end_s/samp_wav@samp.rate
findSpeech <- function(wave,
                       minAmp = 500,
                       maxPause = 10000,
                       beginPad = 2000,
                       endPad = 4000) {
  if (class(wave)[1] != "Wave") {
    wave <- tuneR::readWave(wave)
  }

  env_df <- data.frame(amp = abs(wave@left),
                       env = seewave::env(wave, plot = FALSE),
                       sample = 1:length(wave@left))

  env_min <- env_df %>% dplyr::filter(env >= minAmp)

  markers <- list()
  if (nrow(env_min) < (wave@samp.rate/1000)) {
    markers$end_s <- NA
    markers$begin_s <- NA
  } else {
    env500 <- env_df %>% dplyr::filter(env >= minAmp) %>%
      dplyr::mutate(to_next = dplyr::lead(sample)-sample,
                    to_next = ifelse(is.na(to_next), 1, to_next),
                    count = ifelse(to_next < maxPause, 1, 0),
                    grp = data.table::rleid(count)) %>% dplyr::group_by(grp) %>%
      dplyr::mutate(to_next = ifelse(count == 0, 0, to_next),
                    len = sum(to_next)) %>% dplyr::ungroup() %>%
      dplyr::filter(len == max(len))

    markers$end_s <- if (max(env500$sample)+endPad < max(env_df$sample)) {max(env500$sample) + endPad} else {max(env500$sample)}
    markers$begin_s <- if (min(env500$sample) > beginPad) {min(env500$sample) - beginPad} else {min(env500$sample)}
  }

  return(markers)
}
