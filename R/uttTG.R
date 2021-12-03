#' Create a Praat TextGrid marking speech onset and offset
#'
#' @param wave Required. Either a `tuneR::Wave` object or a valid file path to an existing wav file.
#' @param outfile If not `NULL` (the default), a filename ending in .TextGrid where the output of this function will be stored. If `NULL`, the function will return a character vector including the text to put in a TextGrid file.
#' @param begin Integer. The first SAMPLE in the `wave` to be considered part of the speech signal. If `NULL` (the default), will use the automatically detected value output by findSpeech(wave).
#' @param end Integer. The last SAMPLE in the `wave` to be considered part of the speech signal. If `NULL` (the default), will use the automatically detected value output by findSpeech(wave).
#' @param transcription Character. The transcription of the segment between the `begin` and `end` markers.
#'
#' @return A praat TextGrid file.
#' @export
#'
#' @examples
#' data("samp_wav")
#' uttTG(samp_wav, outfile = "samp_tg.TextGrid",
#'       transcription = c("why what do you mean asked the detective"))
uttTG <- function(wave, outfile,
                  begin = NULL, end = NULL,
                  transcription = NULL){
  if (class(wave)[1] != "Wave") {
    wave <- tuneR::readWave(wave)
  }

  if (is.null(transcription)) {
    transcription <- ""
  }

  if (is.null(begin)|is.null(end)) {
    markers <- findSpeech(wave)
    begin <- markers$begin_s
    end <- markers$end_s
  }

  begin_t <- begin/wave@samp.rate
  end_t <- end/wave@samp.rate
  tg_head <- data.frame(
    tg = as.character(c("File type = \"ooTextFile\"",
                        "Object class = \"TextGrid\"","",
                        0, length(wave@left)/wave@samp.rate,"<exists>",1)))
  utt_tier <- data.frame(
    tg = as.character(c("\"IntervalTier\"", "\"utterance\"",
                        0, length(wave@left)/wave@samp.rate, 3,
                        0, begin_t, "\"\"",
                        begin_t, end_t, paste0("\"", transcription, "\""),
                        end_t, length(wave@left)/wave@samp.rate, "\"\"")))
  txt <- bind_rows(tg_head, utt_tier)
  if (!is.null(outfile)) {
    write.table(txt, file = outfile,
                row.names = FALSE, col.names = FALSE,
                quote = FALSE, sep = ",", qmethod = "double")
  } else {
    return(txt)
  }
}
