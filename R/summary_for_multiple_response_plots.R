#' Function to summarise variable that allows multiple responses from CES and prepare for plotting
#'
#' @param A dataframe containing grouping variables and outcome variable
#' @param ... Grouping variable(s)
#' @param out An outcome variable where more than one response can be selected
#'
#' @return A summary tibble in long format for ggplot of n, total and percentage (n/total)
#' where n is frequency of each level within the outcome variable and total is the sum of n, grouped by grouping variable(s).
#' NAs are included because this function is often applied to follow-up questions to a binary question.
#' If NAs were excluded, reader won't know what the calculated percentage may be skewed to the readers as it becomes unclear
#' what the percentage is expressed on.
#'
#' @export
#'
#' @examples When using function, need to specify out = otherwise function will assume it is a grouping variable
formultipleplot <- function(data,..., out){

  uniqueOptions <- data %>% select({{out}}) %>% unique() %>% drop_na() %>%
    as.matrix() %>%  str_split(., " ") %>% unlist() %>% unique()

  # create columns of binary variables for each response option
  for (k in uniqueOptions) {
    data <- data %>% mutate("{k}" := case_when(str_detect({{out}}, k) ~ 1,
                                               is.na({{out}}) ~ NA_real_,
                                               TRUE ~ 0))

  }

  data %>% group_by(...) %>%
    select(..., all_of(uniqueOptions)) %>%
    summarise(across(c(where(is.numeric)),~sum(.x, na.rm=T))) %>%
    pivot_longer(cols=c(all_of(uniqueOptions)), values_to = "n") %>%
    group_by(...) %>%
    inner_join(., data %>% group_by(...) %>%
                 count({{out}}) %>%summarise(Total=sum(n))) %>%
    mutate(perc = n/Total*100) %>%
    arrange(desc(perc)) %>% ungroup() %>%
    mutate(name = forcats::fct_inorder(name)) %>%
    filter(perc!=0)

}
