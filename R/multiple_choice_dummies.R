#' Function that converts a multiple choice CES question into a dummy matrix of individual answers
#'
#' @param data The data to use. Not in quotes.
#' @param variable The variable with the multiple choice answers. In quotes.
#' @param prefix The prefix to use for the column names of the dummy matrix. In quotes.
#'
#' @return A data frame of the same length as the data frame passed as argument. Has as many columns as there
#'   are unique answers in the variable to review.
#'
#' @export
make_dummies <- function(data, variable, prefix){
  # Creates a dataframe of dummies based on the answers given
  unique_names <- unique(unlist(stringr::str_split(unique(data[[variable]]), " ")))
  unique_names <- as.list(unique_names[!is.na(unique_names)]) # Drop NA cases, make list to feed to map
  result <- purrr::map(unique_names, ~as.double(grepl(sprintf("\\<%s\\>", .x), data[[variable]])))
  # "\\<%s\\>" ensures whole word is matched, no matching "no" in "unknown"
  names(result) <- paste(prefix, unlist(unique_names), sep = "_")
  result <- bind_cols(result)
  return(result)
}
