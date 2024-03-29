#' Equity calcualtion
#'
#' A function to calculate the equity values based on teh Equity Tool values.
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
  df <- equity_file %>%
    pivot_longer(!qns, names_to = c(".value", "set"),  names_pattern = "(op|score)([0-9])",
                 values_drop_na = T) %>%
    select(question = qns, answer = op, score)

  df$code <- paste(df$question, df$answer, sep = " : ")

  # Compare equity df from survey to values and options
  if (!identical(sort(names(data)), sort(unique(df$question)))){
    stop("Col names of HH respondents DF does not correspond with question names from Equity scores DF")
  }

  # Ensure answer codes are the same
  for (q_name in names(data)){

    q_name_s <- rlang::sym(q_name)
    unique_names_data <- unique(data %>% dplyr::select(!!q_name_s) %>%
                                  dplyr::filter(!is.na(!!q_name_s)) %>% dplyr::pull(!!q_name_s))
    unique_names_eq_file <- df %>% dplyr::filter(question == q_name) %>% dplyr::pull(2)
    if (!all(unique_names_data %in% unique_names_eq_file)){
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
