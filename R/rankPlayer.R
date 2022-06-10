#' Get current player's rank among group of past players
#'
#' @param score The player's current score.
#' @param playerId The player's ID number. MUST be unique for each player! Use \code{\link{pinGen}}.
#' @param rankFile A file path to a new or existing .rds or .csv file of previous players' scores. Must contain 3 columns described by the remaining arguments.
#' @param score_col The name of the column in `rankFile` where the past players' scores are stored. Defaults to "score".
#' @param id_col The name of the column in `rankFile` where the past players' unique ID numbers are stored. Defaults to "pin".
#' @param rank_col The name of the column in `rankFile` where the past players' ranks are stored. Defaults to "rank".
#'
#' @return A list with 2 values: (1) rank = the current players rank (integer) and (2) out_of = the total number of players so far.
#' @export
#' @examples
#' # Erase the out file, "sample_rank.rds" after you run this example,
#' # ...so you won't get the error of multiple "players" with the same ID next time you run the example!
#' if (interactive()) {
#'   library(shiny)
#'   library(shinyalert)
#'   library(shinyjs)
#'   ui <- fluidPage(
#'     useShinyalert(),
#'     actionButton("ok", "Click to add to your score!"),
#'     textOutput("score_txt"),
#'     actionButton("rank", "Show your rank when you're ready.")
#'   )
#'
#'   server <- function(input, output, session) {
#'     rv <- reactiveValues(score = 0, pin = 1)
#'     observeEvent(input$ok, {
#'       rv$score <- rv$score + sample(c(1:9), 1)
#'       output$score_txt <- renderText({
#'         paste0("Your current score: ",rv$score)
#'         })
#'       enable("rank")
#'     })
#'
#'     observeEvent(input$rank, {
#'       disable("rank")
#'       rv$pin <- rv$pin+1
#'       rank <- rankPlayer(score = rv$score,
#'                          playerId = rv$pin,
#'                          rankFile = "sample_rank.rds")
#'       text <- paste0("You rank #", rank$rank,
#'                      " out of ", rank$out_of, " player(s).")
#'       shinyalert(title = "Congratulations!",
#'                  type = "success",
#'                  text = text)
#'     })
#'   }
#'   shinyApp(ui = ui, server = server)
#' }
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
                      sc = score_col) %>%
        dplyr::mutate(pin = as.character(pin))
    } else if (grepl("\\.csv$", rankFile)) {
      all <- read.csv(rankFile) %>%
        dplyr::rename(rank = rank_col,
                      pin = id_col,
                      sc = score_col) %>%
        dplyr::mutate(pin = as.character(pin))
    } else {
      stop("Argument 'rankFile' must be a file path to a CSV or RDS file. If the file specified does not exist, it will be created. But the extension must be .csv or .rds.")
    }
  } else {
    all <- data.frame(rank = numeric(), pin = character(), sc = numeric())
  }
  new <- data.frame(rank = NA, pin = as.character(playerId), sc = as.numeric(score))

  both <- dplyr::bind_rows(all, new) %>%
    dplyr::mutate(rank = (nrow(.)+1) - rank(sc, ties.method = "min"))

  out <- both %>% dplyr::select(rank, pin, sc)
  colnames(out) <- c(rank_col, id_col, score_col)

  if (grepl("\\.csv$", rankFile)) {
    write.csv(out, rankFile, row.names = FALSE)
  } else if (grepl("\\.rds$", rankFile)) {
    saveRDS(out, rankFile)
  }
  return(list(rank = both$rank[both$pin == playerId], out_of = nrow(both)))
}
