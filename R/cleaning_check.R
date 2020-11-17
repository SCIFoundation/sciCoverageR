#' Cleaning checks
#'
#' Function showing a summary of the variable and the number of missings for data cleaning
#'
#' @param data The data to use. Not in quotes
#' @param variable The variable to use.formatted as data$variable - in quotes
#' @param tosplitby The variable to split the missings by - in quotes
#' @param makefactor if not NA, makes variable is turned to factor
#'
#' @return If the variable is a character or factor, the number of non-missing levels and
#'   * if there's less than 200 levels - the levels and how many rows in each
#'   * if there's more than 200 levels - the proportion of levels that are unique.
#'   If the variables is numeric returns a summary
#'   The number of missing variables broken down by 'tosplitby'
#'
#' @importFrom magrittr %>%
#' @export
checkingvariables <- function(data, variable, tosplitby, makefactor = NA){
  # get a temp dataset to work with
  data <- as.data.frame(data)
  t.data <- data[,c(variable, tosplitby)]
  t.data$variable <- t.data[, variable]
  t.data$tosplitby <- t.data[, tosplitby]

  print(variable)

  #if character of factor then print unique if less than 200 unique or print number of unique
  if (is.character(t.data$variable) | is.factor(t.data$variable) |
      inherits(t.data$variable, 'Date')  | !is.na(makefactor)){
    if (length(unique(t.data$variable)) < 200) {
      print("num non-missing unique")
      print(length(unique(t.data$variable[!is.na(t.data$variable)])))
      print(t.data %>% dplyr::filter(!is.na(variable)) %>%
              dplyr::group_by(variable) %>%
              dplyr::summarise(n=dplyr::n()) %>%
              dplyr::arrange(dplyr::desc(n)))

    } else {
      print("num unique")
      print(length(unique(t.data$variable)))
      print("num in total")
      print(length(t.data$variable))
      print("proportion unique")
      print(length(unique(t.data$variable)) / length(t.data$variable))
    } #if numeric then print summary
  } else if (is.numeric(t.data$variable)){
    print(summary(t.data$variable))
  }
  #print a breakdown of the missings
  if (sum(is.na(t.data$variable)) == 0){
    print("no missing")
  } else{
    print("num missings")
    print(length(t.data$variable[is.na(t.data$variable)]))
    print(t.data %>% dplyr::filter(is.na(variable)) %>%
            dplyr::group_by(tosplitby) %>%
            dplyr::summarise(n=dplyr::n()) %>%
            dplyr::arrange(dplyr::desc(n)))
  }
}
