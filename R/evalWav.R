#' Check a WAV file's quality
#'
#' @param wave Required. Either a `tuneR::Wave` object or a valid file path to an existing wav file.
#' @param utteranceTG If not `NULL` (the default), a filename for the Praat TextGrid to be created. Must have the extension `.TextGrid`.
#' @param transcription If not `NULL` (the default), a character vector indicating the transcription of the utterance.
#' @param trim If not `NULL` (the default), a filename for the wav file to be created from the output of findSpeech() and the original .wav denoted with argument `wave`.
#' @param plotOsc If not `NULL` (the default), a character vector that will server as the title of the wave form plot (recommended = the name of the object in `wave`).
#'
#' @return A list with 7 elements: the markers for the beginning and end of the utterance in samples, the sample rate of the original file, the SNR (dB), a logical value indicating whether 1% of samples in the original wave file were clipped, a tuneR::Wave object for the trimmed file (if `trimmed != NULL`), the text of the Praat TextGrid (if `utteranceTG != NULL`), and the plot of the wave form beginning and end markers (if `plot != NULL`).
#'
#' @examples
#' data("samp_wav")
#' eval <- evalWav(samp_wav, plotOsc = "samp_wav")
#' print(eval$plot)
evalWav <- function(wave,
                    utteranceTG = NULL,
                    transcription = NULL,
                    trim = NULL,
                    plotOsc = NULL) {
  if (isTRUE(shiny)) {
    if (onFail != "continue" & onFail != "stop")
      stop("If shiny=TRUE, onFail must be 'continue' or 'stop.'")
  }

  #if (!is.null(utteranceTG) & !grepl("\\.TextGrid$", utteranceTG))
    #stop("You must give a valid filepath ending in .TextGrid for argument 'filenameTG'.")

  #if (!is.null(trim) & !grepl("\\.wav$", trim))
    #stop("You must give a valid filepath ending in .wav for argument 'trim'.")

  if (class(wave)[1] != "Wave") {
    wave <- tuneR::readWave(wave)
  }

  markers <- findSpeech(wave)

  snr <- getSNR(wave,
                begin_s = markers$begin_s,
                end_s = markers$end_s)

  clipped <- clipCheck(wave, return = "logical")


  if (!is.null(utteranceTG)) {
    tg <- uttTG(wave, begin = markers$begin_s,
                          end = markers$end_s,
                          outfile = utteranceTG)
  } else {
    tg <- NULL
  }

  if (!is.null(trim)) {
    trimmed <- trimWav(wave, begin = markers$begin_s,
                          end = markers$end_s,
                          outfile = trim)
  } else {
    trimmed <- NULL
  }

  if (!is.null(plotOsc)) {
    df <- data.frame(amplitude = as.numeric(wave@left),
                     time = (1:length(wave@left))/wave@samp.rate)
    t1 <- round((markers$begin_s/wave@samp.rate), digits = 3)
    t2 <- round((markers$end_s/wave@samp.rate), digits = 3)
    subtitle <- paste0("Beginning = ", t1, "s. End = ", t2, "s.")
    title <- paste0(plotOsc)
    plot <- ggplot2::ggplot(df, ggplot2::aes(x = time, y = amplitude)) +
      ggplot2::geom_path() + ggplot2::geom_vline(ggplot2::aes(xintercept = t1),
                               color = "green") +
      ggplot2::geom_vline(ggplot2::aes(xintercept = t2), color = "red") +
      ggplot2::labs(title = title, subtitle = subtitle)
  } else {
    plot <- NULL
  }

  return(list(
    markers = markers,
    samp.rate = wave@samp.rate,
    snr = snr,
    clipped = clipped,
    trimmed = trimmed,
    tg = tg,
    plot = plot
  ))
}
