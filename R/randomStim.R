#' Randomize Trials
#'
#' @param dataFile A data.frame, RDS file, or CSV file with, minimally, a column containing all the stimuli for the experiment. Each row is treated as one trial.
#' @param blockCol The name of the column in dataFile that will serve as a key, noting which columns belong to which blocks.
#' @param n_practice Required. Integer. The number of practice trials included in dataFile. If present, practice trials must always occupy the first row(s) of dataFile. If no practice trials are included, set this argument to 0 (the default).
#' @param n_perBlock Integer. If blockCol = NULL, the number of rows that should be treated as a block in dataFile.
#' @param n_blocks Integer. If blockCol = NULL, the number of blocks (not counting practice trials) in the experiment.
#' @param blocksSameOrd Boolean. Should the stimuli be in the same random order in all blocks? Defaults to TRUE.
#' @param outFile If not NULL, the file name (CSV or RDS) for writing the resulting randomized data to.
#'
#' @return A data frame containing the columns from dataFile, as well as a column 'block' denoting the block to which each trial belongs, and a column 'trial' denoting the row's order within the block. The number of rows is equal to the number of practice trials + all experimental trials from all blocks.
#' @details Please read the additional information about each of the following arguments below:
#' \itemize{
#'   \item{dataFile}{This must be a data.frame (or file path to an object that can be coerced to a data.frame). It should contain all the necessary pieces of data for each trial, with each piece in its own column. Rows represent trials. For example, if you have a target and three distractors, you should have the columns 'target', 'distractor1', 'distractor2', and 'distractor3', in dataFile. If each trial belongs to a condition, this information should also be included in a separate column. IMPORTANT: dataFile must have either the same number of rows as practice + experimental trials in the experiment OR the number of rows equal to the number of practice trials + the number of trials in a block. Note that to use the latter option, all blocks of experimental trials MUST each contain the same number of trials. ANOTHER IMPORTANT NOTE: Practice trials must be listed BEFORE any experimental trials as the first rows of the data.frame.}
#'   \item{blockCol}{If not NULL, this should be the name of a column in dataFile, that shows which trials belong to which blocks. The values in blockCol are usually numeric, but may be any type, as long as the values are distinct for each block. NOTE: this column will be replaced by the column 'block' in the returned randomized dataset.}
#'   \item{n_practice}{This argument is required, regardless of whether blockCol is present.}
#'   \item{n_blocks} and \item{n_perBlock}{If blockCol is NULL, these values are required. Do NOT use these values if you wish to have variable numbers of trials per block (in that case, use blockCol and be sure dataFile has the same number of rows as trials in the entire experiment.)}
#'   \item{blocksSameOrd}{If this argument is TRUE, all blocks of experimental trials MUST have the same number of items per block. The trials within each block will be randomized, and the same random order will be used for all blocks.}
#'   \item{outFile}{This argument is useful when you need to use the same random order across all participants, or, if the argument is supplied with a dynamic value, when you wish to save a copy of the order used for each participant. The latter is useful if participants pause the experiment and resume in a different Shiny session (after, for example, internet failure).}
#' }
#'
#' @export
#' @examples
#' library(shiny)
#' library(dplyr)
#'
#' if (interactive()) {
#'   ui <- fluidPage(
#'     fluidRow(
#'       column(width = 8, offset = 2,
#'              actionButton("start", "Start"),
#'              uiOutput("trial"),
#'              hr(),
#'              h4(textOutput("feedback"))
#'       )
#'     )
#'   )
#'
#'   server <- function(input, output, session) {
#'     pin <- pinGen(file = "pinlist.rds",
#'                   reactive = FALSE)
#'     counter <- reactiveValues(trial = 1, block = 0,
#'                               per_block = 1,
#'                               pin = pin)
#'     rvs <- reactiveValues()
#'     trial_data <- data.frame(target = c("hayed", rep.int(c("heed", "hid", "head"), 3)),
#'                              distractor1 = c("hoed", rep.int(c("hid", "head", "heed"), 3)),
#'                              distractor2 = c("hawed", rep.int(c("head", "heed", "hid"), 3)),
#'                              block = c(0, unlist(mapply(rep, c(1:3), 3))))
#'     randomized <- randomStim(dataFile = trial_data,
#'                              blockCol = "block",
#'                              n_practice = 1,
#'                              blocksSameOrd = FALSE,
#'                              outFile = paste0("randomized", pin, ".rds"),
#'                              reactive = FALSE)
#'
#'     observe({
#'       stim_row <- randomized %>%
#'         filter(trial == counter$trial & block == counter$block)
#'       rvs$targ <- stim_row$target[1]
#'       rvs$dis1 <- stim_row$distractor1[1]
#'       rvs$dis2 <- stim_row$distractor2[1]
#'       counter$per_block <- randomized %>%
#'         filter(block == counter$block) %>% nrow()
#'     })
#'
#'     observeEvent(input$start, {
#'       choices <- c(sample(c(rvs$targ, rvs$dis1, rvs$dis2), 3))
#'
#'       output$trial <- renderUI({
#'         radioButtons(inputId = "task",
#'                      label = HTML(paste0("<h3>Select the word target word, <em>", rvs$targ, "</em>, from the choices below.</h3>")),
#'                      choiceNames = choices,
#'                      choiceValues = choices,
#'                      selected = character())
#'       })
#'     })
#'
#'     observeEvent(input$task, {
#'       if (input$task == rvs$targ) {
#'         if (counter$trial == counter$per_block) {
#'           counter$block <- counter$block + 1
#'           counter$trial <- 1
#'         } else {
#'           counter$trial <- counter$trial + 1
#'         }
#'         output$feedback <- renderText({
#'           paste0("Great job, player #", counter$pin, "! You found the correct answer. Click 'Start' to play again.")
#'         })
#'       } else {
#'         output$feedback <- renderText({
#'           paste0("Wrong answer, player #", counter$pin, "! Click 'Start' to play again.")
#'         })
#'       }
#'     })
#'   }
#'   shinyApp(ui = ui, server = server)
#' }
randomStim <- function(dataFile,
                       blockCol = NULL,
                       n_practice = 0,
                       n_perBlock = NULL,
                       n_blocks = NULL,
                       blocksSameOrd = FALSE,
                       outFile = NULL) {
  if (!is.data.frame(dataFile)){
    if (!grepl("csv$", dataFile) & !grepl("rds$", dataFile))
      stop("Argument 'dataFile' must be an object of class data.frame, or a path to an existing 'csv' or 'rds' file.")
    if (!file.exists(dataFile))
      stop(paste0("The file '", dataFile, "' does not exist"))
  }
  if (!is.null(outFile) & !grepl("csv$", outFile) & !grepl("rds$", outFile))
    stop("Argument 'outFile' must be of the format '.csv' or '.rds'.")
  if (is.null(blockCol) & !is.integer(n_practice))
    stop("Argument 'n_practice' must be a positive integer representing the number of practice trials.")
  if (is.null(blockCol) & !is.integer(n_perBlock))
    stop("Argument 'n_perBlock' must be a positive integer representing the number of trials per block.")
  if (is.null(blockCol) & !is.integer(n_blocks))
    stop("Argument 'n_blocks' must be a positive integer representing the number of blocks in the experiment.")
  if (is.null(blockCol) & is.null(n_perBlock) & is.null(n_blocks))
    stop("You must specify either a column of `dataFile` as a key for which stimuli belong to which blocks, or numbers of stimuli and blocks using the `n_perBlock` and `n_blocks` arguments.")

  if (!is.data.frame(dataFile)) {
    if (grepl("csv$", dataFile)) {
      df_init <- read.csv(dataFile)
    } else if (grepl("rds$", dataFile)){
      df_init <- readRDS(dataFile)
      }
  } else {
    df_init <- dataFile
  }

  if (!is.null(blockCol) & !blockCol %in% colnames(df_init))
    stop("Argument `blockCol` must be equal to the name of a column in the input datafile.")

  if (!is.null(blockCol)) {
    grps <- df_init %>%
      dplyr::group_by(.data[[blockCol]]) %>%
      dplyr::mutate(b_count = n()) %>%
      dplyr::select(.data[[blockCol]], b_count) %>%
      unique()
    if (n_practice > 0) {
      n_perBlock <- c(unique(grps$b_count[2:nrow(grps)]))
      n_blocks <- nrow(grps)-1
    } else {
      n_perBlock <- c(unique(grps$b_count))
      n_blocks <- nrow(grps)
    }
  } else {
    if (n_practice > 0) {
      grps <- data.frame(block = c(0, 1:n_blocks),
                         b_count = c(n_practice,
                                     rep.int(n_perBlock, n_blocks)))
    } else {
      grps <- data.frame(block = c(1:n_blocks),
                         b_count = rep.int(n_perBlock, n_blocks))
    }
  }

  if ((length(n_perBlock) > 1) & is.null(blockCol))
    stop("If you wish to have variable numbers of trials per block, you must give a key as to which stimuli belong to which blocks using `blockCol`, and then number of rows in `datafile` must be equal to the total number of practice + experimental trials across all blocks.")

  if ((length(n_perBlock) > 1) & (nrow(df_init) != (n_practice + (n_perBlock*n_blocks))))
    stop("If you wish to have variable numbers of trials per block, you must give a key as to which stimuli belong to which blocks using `blockCol`, and then number of rows in `datafile` must be equal to the total number of practice + experimental trials across all blocks.")

  if (isTRUE(blocksSameOrd)) {
    #if (length(n_perBlock) > 1 | !is.integer((n_blocks*n_perBlock)/(nrow(df_init)-n_practice)))
      #stop("To use the same order across blocks, the number of experimental trials must be the same across blocks and the same as or an integer multiple of the total number of rows in `dataFile` minus `n_practice`.")
    ord_df <- data.frame(block = unlist(mapply(rep,
                                               c(grps$block),
                                               grps$b_count)),
                         ord = c(seq.int(length.out = n_practice),
                                 rep(sample(c(1:n_perBlock),
                                            size = n_perBlock),
                                     times = n_blocks)))
  } else {
    ord_df <- data.frame(block = unlist(mapply(rep,
                                            c(grps$block),
                                            grps$b_count)),
                      ord = unlist(sapply(grps$b_count, function(i){
                        sample(c(1:grps$b_count[i]), grps$b_count[i])
                      })))
  }

  if (nrow(df_init) < (n_practice + (n_perBlock*n_blocks))) {
    exper <- df_init %>% dplyr::filter(row_number()>n_practice)
    b_list <- replicate((n_blocks-1), exper, simplify = FALSE)
    all_trials <- dplyr::bind_rows(df_init, b_list)
  } else {
    all_trials <- df_init
  }

  if ("block" %in% colnames(all_trials)) {
    all_trials <- all_trials %>% dplyr::select(-block)
  }

  df_end <- dplyr::bind_cols(all_trials, ord_df) %>%
    dplyr::arrange(block, ord) %>% dplyr::rename(trial = ord)

  if (!is.null(outFile)) {
    if (grepl("csv$", outFile)) {
      write.csv(df_end, outFile, row.names = FALSE)
    } else {
      saveRDS(df_end, outFile)
    }
  }
  return(df_end)
}
