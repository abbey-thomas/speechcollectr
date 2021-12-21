rankPlayer <- function(score, playerId = NULL,
                       rankFile,
                       score_col = "score",
                       id_col = "pin",
                       rank_col = "rank") {
  if (file.exists(rankFile)) {
    if (grepl("\\.rds$", rankFile)) {
      all <- readRDS(rankFile) %>%
        dplyr::rename(rank = rank_col,
                      pin = id_col,
                      sc = score_col)
    } else if (grepl("\\.csv$", rankFile)) {
      all <- read.csv(rankFile) %>%
        dplyr::rename(rank = rank_col,
                      pin = id_col,
                      sc = score_col)
    } else {
      stop("Argument 'rankFile' must be a file path to a CSV or RDS file. If the file specified does not exist, it will be created. But the extension must be .csv or .rds.")
    }
  } else {
    all <- data.frame(rank = numeric(), pin = character(), sc = numeric())
  }
  new <- data.frame(rank = NA, pin = playerId, sc = as.numeric(score))

  both <- dplyr::bind_rows(all, new) %>%
    dplyr::mutate(rank = rank(sc, ties.method = "min"))

  out <- both %>% dplyr::select(rank, pin, sc)
  colnames(out) <- c(rank_col, id_col, score_col)

  if (grepl("\\.csv$", rankFile)) {
    write.csv(out, rankFile, row.names = FALSE)
  } else if (grepl("\\.rds$", rankFile)) {
    saveRDS(out, rankFile)
  }
  return(list(rank = both$rank[both$pin == playerId], out_of = nrow(both)))
}
