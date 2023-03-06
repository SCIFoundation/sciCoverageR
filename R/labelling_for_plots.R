#' Function for converting variable values to labels suitable for reporting and graphs.
#' It replaces all underscores with whitespaces and fixes whitespaces,
#' removes numbering options at the start of the string,
#' replaces the following abbreviations with the correct terms (CWT=community-wide, SBT=school-based, CDD=medicines distributors, Abd pain=abdominal),
#' It also converts variable into factor for plotting, and 'other' options are always ordered last.
#'
#'
#' @param dataset A dataframe containing the variables to be relabelled
#' @param ... Variable(s) to be relabelled
#' @param nchar Maximum number of characters in each line (default is 20)
#'
#' @return An object of the same type as `dataset` with specified variable(s) as factor(s) with appropriate labelling.
#' @export
#'
#' @examples dataset %>% label_for_plot(vil_mda_type)
label_for_plot <- function(dataset, ..., nchar=20){

  vars_to_label <- ensyms(...)

  suppressWarnings(dataset %>%
                     mutate(across(c(!!!vars_to_label), ~ str_replace_all(., "_", " "))) %>%
                     mutate(across(c(!!!vars_to_label), ~ str_replace(.,"[[:digit:]]{0,2}+", ""))) %>%

                     mutate(across(c(!!!vars_to_label), ~ str_squish(.))) %>%
                     mutate(across(c(!!!vars_to_label), ~ str_replace_all(., "CWT", "Community-wide")))  %>%
                     mutate(across(c(!!!vars_to_label), ~ ifelse(grepl("(SBT )",.),str_replace_all(.,"SBT","School-based &"),str_replace_all(.,"SBT","School-based"))))  %>%
                     mutate(across(c(!!!vars_to_label), ~ str_replace_all(., "door2door", "door to door")))  %>%
                     mutate(across(c(!!!vars_to_label), ~ str_replace_all(., "sideeffects", "side effects")))  %>%
                     mutate(across(c(!!!vars_to_label), ~ str_replace_all(., "villageheadhouse", "village head house")))  %>%
                     mutate(across(c(!!!vars_to_label), ~ str_replace_all(., "CDD", "medicines distributor")))  %>%
                     mutate(across(c(!!!vars_to_label), ~ str_replace_all(., "abd pain", "abdominal pain")))  %>%
                     mutate(across(c(!!!vars_to_label), ~ gsub("^(Na)$|\\b(mda|tdm|dmm|Pzq|Mbd|Alb|Ivm|Sac|Wra)\\b", "\\U\\1\\U\\2", str_to_sentence(.), perl=T, ignore.case = T))) %>%
                     mutate(across(c(!!!vars_to_label), ~ str_wrap(., width = nchar))) %>%
                     mutate(across(c(!!!vars_to_label), ~ forcats::fct_inorder(.)))%>%


                     mutate(across(c(!!!vars_to_label), ~ forcats::fct_relevel(., "Other", after = Inf)))%>%
                     mutate(across(c(!!!vars_to_label), ~ forcats::fct_relevel(., "Autre", after = Inf)))%>%
                     mutate(across(c(!!!vars_to_label), ~ forcats::fct_relevel(., "NA", after = Inf))))
}
