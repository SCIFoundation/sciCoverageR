#' A function
#'
#' @param data A data frame of HHs (more details)
#' @param vectorOfQuintiles A vector (more details)
#' @param equity_file A data frame (more details)
#'
#' @return A data frame containing the equity scores and the equity quintiles for each household of data (first parameter)
#'
#' @importFrom magrittr %>%
#' @export
equity_calculation <- function(data, vectorOfQuintiles, equity_file){

  # Amended Equity Tool response data frame
  df <- dplyr::bind_rows(equity_file %>% dplyr::select(1, answer = 2, score = 3),
                         equity_file %>% dplyr::select(1, answer = 4, score = 5),
                         equity_file %>% dplyr::select(1, answer = 6, score = 7)) %>%
    stats::na.omit()
  names(df) <- c("question", "answer", "score")
  df$code <- paste(df$question, df$answer, sep = " : ")

  # Compare equity df from survey to values and options
  if (!identical(sort(names(data)), sort(unique(df$question)))){
    stop("Col names of HH respondents DF does not correspond with question names from Equity scores DF")
  }

  # Ensure answer codes are the same
  for (q_name in names(data)){
    if (!all(unique(data[[q_name]])[!is.na(unique(data[[q_name]]))] %in%
             tibble::deframe(df[df$question == q_name, 2]))){
      stop(sprintf("The answer codes for var %s in the HH respondents DF and the Equity scores DF do not match", q_name))
    }
  }

  # Transpose equity questions data so that each individual is a column
  equity_transpose <- as.data.frame(t(data), stringsAsFactors = F)
  # Add the question category to the response to make responses unique for each individual
  equity_transpose_codes <- purrr::map(as.list(seq(nrow(data))),
                                       ~ paste(rownames(equity_transpose), equity_transpose[, .x], sep = " : "))
  names(equity_transpose_codes) <- paste("hh", seq(length(equity_transpose_codes)), sep = "_")
  equity_transpose_codes <- dplyr::bind_cols(equity_transpose_codes)

  # Scores for the individual are the sum of the values the responses
  score_vector <- purrr::map_dbl(equity_transpose_codes, ~ sum(df$score[match(.x, df$code)]))

  # Assign quintile based on vector of quintiles
  quintile_vector <- cut(score_vector, c(-Inf, vectorOfQuintiles, Inf), labels = c(1:5))

  # Return a data frame with scores and quintiles as output
  result <- data.frame(scores = score_vector, quintiles = quintile_vector, row.names = NULL)
  return(result)
}
