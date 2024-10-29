#' Summarise a 'select one' variable for plotting
#'
#' Function to summarise variables from CES and prepare for plotting
#'
#' @param data A dataframe containing grouping variables and outcome variable
#' @param ... Grouping variable(s)
#' @param out An outcome variable. Specify out = otherwise function will assume it is a grouping variable
#' @param dropna Should NAs be dropped from dataframe?
#' @param number Arrange descending by number if outcome variable numeric?
#'
#' @return A summary tibble in long format for ggplot of n, total and percentage (n/total)
#' where n is frequency of each level within the outcome variable and total is the sum of n, grouped by grouping variable(s)
#' @export
#'
forplot <- function(data, ..., out,dropna = c(FALSE,TRUE), number= c(FALSE,TRUE)){
  if (missing(dropna)) dropna <- FALSE
  else dropna <- dropna
  if (missing(number)) number <- FALSE
  else number <- number
  #dropna = assertive::use_first(dropna) #assertive is deprecated
  #number= assertive::use_first(number)
  data %>%  group_by(...) %>% count({{out}}) %>%
    {if(dropna==TRUE) drop_na(.)  else .} %>%
    mutate(Total=sum(n, na.rm=T),
           perc=n/Total*100) %>%

    {if(number==TRUE) {.} %>% arrange(desc(n)) else {.} %>% arrange(desc(perc))} %>%
    ungroup() %>%

    {if(is.factor(pull(.,{{out}}))|is.character(pull(.,{{out}}))) mutate(., {{out}} := forcats::fct_inorder({{out}})) else .}
}

