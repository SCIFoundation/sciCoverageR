#' A function to calculate the p values of differences between sexes and attend/non-attend SAC.
#'
#' @param drug_name The three letter code for the drug (e.g., pzq, alb, ...). In quotes.
#' @param group The code for the group of interest (e.g., "1_SAC", "2_Adults"). In quotes.
#' @param partition The variable to determine by what category the data will be split by. In quotes.
#' @param data The data from the survey. A dataframe, not in quotes.
#'
#' @return A data frame containing the p values for the different differences for the chosen drug and group.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang :=
#' @export
calculate_p_values <- function(drug_name, group, partition, data){

  #==================================================================================#
  # Check vars in data - No output, just throws error if data not according to dictionary
  #==================================================================================#

  evaluate_data(data)

  #==================================================================================#
  # Check drug name - Throws error if name is not in data, otherwise gives var_for_reduction
  #==================================================================================#

  swallow_vars <- names(data)[grepl("^ind_swallowed_[a-zA-Z]+_bin", names(data))]
  present_vars <- unique(gsub('^(ind_)(.+)(_{1})(.+)(_bin)$',"\\4", swallow_vars))
  drug_check <- drug_name %in% present_vars

  if (!drug_check) { # Give warning if drug_name passed to function does not coincide with drug in data
    stop(sprintf("Drug %s is not in the data frame you passed. It contains %s.",
                 drug_name, paste(present_vars, collapse = ", ")))
  }

  var_for_reduction <- swallow_vars[grepl(paste("_", drug_name, "_", sep = ""), swallow_vars)]

  #==================================================================================#
  # Write formulas
  #==================================================================================#

  list_formulas <- write_formulas(drug_name = drug_name, group = group)

  #==================================================================================#
  # Reduce data to partitions relevant to drug passed
  #==================================================================================#

  # First convert string to symbol, for use in dplyr pipe
  part_sym <- rlang::sym(partition)
  reduction_sym <- rlang::sym(var_for_reduction)

  # Find out which partitions have at least one non NA answer
  relevant_partitions <- data %>% dplyr::group_by(!!part_sym) %>%
    dplyr::summarise(count = sum(!is.na(!!reduction_sym))) %>%
    dplyr::filter(count > 0) %>% dplyr::pull(!!part_sym)

  # Reduce
  df <- data %>% dplyr::filter(!!part_sym %in% relevant_partitions)

  #==================================================================================#
  # Reduce data to partitions relevant group
  #==================================================================================#

  df <- df %>% dplyr::filter(ind_group == group)

  #==================================================================================#
  # Calculate p value for given drug, group on reduced data for partitions
  #==================================================================================#

  p_values <- list()
  for (relevant_partition in relevant_partitions){

    p_values[[relevant_partition]] <- list_formulas %>%
      purrr::map(~coeff_extractor(as.formula(.x),
                                  data = df %>% dplyr::filter(!!part_sym %in% relevant_partition))) %>%
      unlist() %>% tibble::enframe()
  }

  p_values <- dplyr::bind_rows(p_values, .id = partition)
  names(p_values) <- c("partition", "case", "p_value")
  p_values <-  p_values %>%
    dplyr::mutate(group = gsub('^(\\d{1})(_{1})([A-Za-z]+)$',"\\3", group),
                  drug = gsub('^([A-Za-z]{3})(_{1})([A-Za-z]{3})(_{1})([A-Za-z]{3})$',"\\1", case),
                  drug = toupper(drug),
                  item = gsub('^([A-Za-z]{3})(_{1})([A-Za-z]{3})(_{1})([A-Za-z]{3})$',"\\5", case),
                  item = paste0("p_diff_", item), # Make it neater
                  question = gsub('^([A-Za-z]{3})(_{1})([A-Za-z]{3})(_{1})([A-Za-z]{3})$',"\\3", case),
                  question = ifelse(question == "off", "reach", "coverage")) %>%
    dplyr::select(partition, group, drug, item, question, p_value)

  #==================================================================================#
  # Return data frame
  #==================================================================================#

  return(p_values)

}

#==================================================================================#
# Add - Check vars in data frame -----
#==================================================================================#

# Check the variables in the data frame used to group (SAC or adult) and disaggregate (sex, school attendance)
# conform to the data dictionary
evaluate_data <- function(data){

  # Check whether data contains variable informing of group (SAC & Adults)
  # Currently the data dicitonary has it as var "ind_group", as this changes, this function would need to change
  if (!("ind_group" %in% names(data))) stop("Function requires the variable ind_group denoting group (e.g., SAC, Adult)")

  # Further, the variable ind_group is written as code << 1_SAC >> for SAC, code << 2_Adult >> for adults.
  # Check at least one of these is contained. As the data dictionary evolves, this would need to reflect those changes
  survey_groups <- unique(stats::na.omit(data$ind_group))
  if (!any(c("1_SAC", "2_Adult") %in% survey_groups)) {
    stop(sprintf("The function assumes var ind_group is coded as << 1_SAC >> for SAC, code << 2_Adult >> for adults. Currently, var ind_group has answers: %s",
                 paste(survey_groups, collapse = ", ")))
  }

  # Check for sex and school attendance variables
  if (!("ind_sex" %in% names(data))) stop("Function requires the variable ind_sex denoting sex")
  if (!("ind_child_attend_bin" %in% names(data))) stop("Function requires the variable ind_child_attend_bin denoting attendance")

  if (!any(c("1_Male", "2_Female") %in% unique(stats::na.omit(data$ind_sex)))) {
    stop(sprintf("The function assumes var ind_sex is coded as << 1_Male >> for males, << 2_Female >> for females. Currently, var ind_sex has answers: %s",
                 paste(unique(stats::na.omit(data$ind_sex)), collapse = ", ")))
  }

  if ("1_SAC" %in% survey_groups){ # Evaluate child attendance variable only if there are kids in the survey
    if (!any(0:1 %in% unique(stats::na.omit(data$ind_child_attend_bin)))) {
      stop(sprintf("The function assumes var ind_child_attend_bin is coded as 0 for males, 1 for females. Currently, var ind_child_attend_bin has answers: %s",
                   paste(unique(stats::na.omit(data$ind_child_attend_bin)), collapse = ", ")))
    }
  }

}

#==================================================================================#
# Add - Write formula ------
#==================================================================================#

write_formulas <- function(drug_name, group){ # writes the possible combinations of formulas

  if(group == "1_SAC"){

    forms <- c(off_sex = "ind_offered_ddd_bin ~ ind_sex + (1|segment_name) + (1|hh_house_code)",
               cov_sex = "ind_swallowed_ddd_bin ~ ind_sex + (1|segment_name) + (1|hh_house_code)",
               off_att = "ind_offered_ddd_bin ~ ind_child_attend_bin + (1|segment_name) + (1|hh_house_code)",
               cov_att = "ind_swallowed_ddd_bin ~ ind_child_attend_bin + (1|segment_name) + (1|hh_house_code)")
    forms_list <- as.list(gsub(pattern = "_ddd_", replacement = paste("_", drug_name, "_", sep = ""), x = forms))
    names(forms_list) <- paste(drug_name, names(forms_list), sep = "_")

  } else if (group == "2_Adult"){

    forms <- c(off_sex = "ind_offered_ddd_bin ~ ind_sex + (1|segment_name) + (1|hh_house_code)",
               cov_sex = "ind_swallowed_ddd_bin ~ ind_sex + (1|segment_name) + (1|hh_house_code)")
    forms_list <- as.list(gsub(pattern = "_ddd_", replacement = paste("_", drug_name, "_", sep = ""), x = forms))
    names(forms_list) <- paste(drug_name, names(forms_list), sep = "_")

  }

  return(forms_list)
}



#==================================================================================#
# Add - p value coefficient extractor ------
#==================================================================================#

coeff_extractor <- function(formula, data){

  # Run the regression as try(...)
  coeff <- try(summary(lme4::glmer(formula, data = data, family= stats::binomial(link = "logit"),
                                   control = lme4::glmerControl(optimizer="Nelder_Mead",
                                                                optCtrl = list(maxfun = 500000)))))
  # If regression throws an error (e.g., model did not converge, write NA as value)
  if (inherits(coeff, "try-error") ) {
    coeff <- NA
    # Otherwise calculate
  } else {
    coeff <- round(coeff$coefficients[2,4], 3)
  }

  return(coeff)

}
