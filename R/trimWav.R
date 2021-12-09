#' Trim leading and trailing noise in a recording containing a single utterance
#'
#' @param wave Required. Either a `tuneR::Wave` object or a valid file path to an existing wav file.
#' @param outfile If not `NULL` (the default), a filename ending in .wav where the trimmed recording will be stored. If `NULL`, the function will return a tuneR::Wave object.
#' @param begin Integer. The first sample in the `wave` to be considered part of the speech signal. If `NULL` (the default), will use the automatically detected value output by findSpeech(wave).
#' @param end Integer. The last sample in the `wave` to be considered part of the speech signal. If `NULL` (the default), will use the automatically detected value output by findSpeech(wave).
#' @param beginPad Integer. How many milliseconds of silence (zeros) should we add to the beginning of the of the detected speech signal?
#' @param endPad Integer. How many milliseconds of silence (zeros) should we add to the end of the detected speech signal?
#'
#' @return A wav file (or tuneR::Wave object if outfile=NULL) trimmed from the original object denoted by argument `wave`.
#'
#' @examples
#' data("samp_wav")
#' head(samp_wav@left)
#' trimmed <- trimWav(samp_wav)
#' head(trimmed@left)
trimWav <- function(wave, outfile = NULL,
                    begin = NULL,
                    end = NULL,
                    beginPad = 50,
                    endPad = 50) {
  if (class(wave)[1] != "Wave") {
    wave <- tuneR::readWave(wave)
  }

  if (is.null(begin)|is.null(end)) {
    markers <- findSpeech(wave)
    begin <- markers$begin_s
    end <- markers$end_s
  }
  pad_b <- ceiling(beginPad/1000*wave@samp.rate)
  pad_e <- ceiling(endPad/1000*wave@samp.rate)

  wave@left <- wave@left[begin:end]
  wave@left <- c(rep_len(0, pad_b), wave@left, rep_len(0, pad_e))
  if (!is.null(outfile)) {
    if (grepl("\\.wav$", outfile))
      stop("Outfile must be a .wav filename.")
    tuneR::writeWave(wave, outfile)
  } else {
    return(wave)
  }

}
