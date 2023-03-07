#' Summarise a 'select one' variable for plotting
#'
#' Function to summarise variables from CES and prepare for plotting
#'
#' @param data A dataframe containing grouping variables and outcome variable
#' @param ... Grouping variable(s)
#' @param out An outcome variable
#' @param dropna Should NAs be dropped from dataframe?
#'
#' @return A summary tibble in long format for ggplot of n, total and percentage (n/total)
#' where n is frequency of each level within the outcome variable and total is the sum of n, grouped by grouping variable(s)
#' @export
#'
#' @examples When using function, need to specify out = otherwise function will assume it is a grouping variable
forplot <- function(data, ..., out,dropna = c(FALSE,TRUE)){
  dropna = assertive::use_first(dropna)
  data %>%  group_by(...) %>% count({{out}}) %>%
    {if(dropna==TRUE) drop_na(.)  else .} %>%
    mutate(Total=sum(n, na.rm=T),
           perc=n/Total*100) %>%
    arrange(desc(perc)) %>% ungroup() %>%

    purrr::when(is.factor(pull(.,{{out}}))  ~ mutate(., {{out}} := forcats::fct_inorder({{out}})), ~ .)

}
