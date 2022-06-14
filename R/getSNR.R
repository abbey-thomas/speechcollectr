#' Get the signal-to-noise ratio of a recording containing a single utterance and some leading or trailing noise
#'
#' @param wave Required. Either a `tuneR::Wave` object or a valid file path to an existing wav file.
#' @param begin Integer. The first sample in the `wave` to be considered part of the speech signal. If `NULL` (the default), will use the automatically detected value output by findSpeech(wave).
#' @param end Integer. The last sample in the `wave` to be considered part of the speech signal. If `NULL` (the default), will use the automatically detected value output by findSpeech(wave).
#'
#' @return The signal-to-noise ratio of the recording in dB.
#' @export
#'
#' @examples
#' data("samp_wav")
#' fs <- findSpeech(samp_wav)
#' snr <- getSNR(samp_wav, fs$begin_s, fs$end_s)
#' snr
getSNR <- function(wave,
                   begin,
                   end) {
  if (class(wave)[1] != "Wave") {
    wave <- tuneR::readWave(wave)
  }

  if (is.null(begin)|is.null(end)) {
    markers <- findSpeech(wave)
    begin <- markers$begin_s
    end <- markers$end_s
  }

  s <- wave@left[begin:end]

  n1 <- c(0:(begin+1))
  n2 <- c((end-1):length(wave@left))
  if (length(n1) >= length(n2)) {
    n <- wave@left[0:begin+1]
  } else {
    n <- wave@left[(end-1):length(wave@left)]
  }

  rms_n <- seewave::rms(n)
  rms_s <- seewave::rms(s)

  snr <- 20*log10((rms_s/rms_n))
  return(snr)
}
