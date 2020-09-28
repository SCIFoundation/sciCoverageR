#' A function to obtain binary var (e.g., coverage: yes / no) estimates with CI from WHO method CS clean data.
#'
#' @param var A string denoting the name of the variable to analyse.
#' @param iu A string denoting the name of the variable to use for administrative disaggregation .
#' @param design A survey package design object.
#'
#' @return A data frame containing the survey estimates for the inputted variable over the inputted administrative disaggregation (commonly the implementation unit) for the survey groups (SAC and/or adults) over all individuals, disaggregated by sex and (for SAC) by school attendance.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang :=
#' @export
estimate_cs_values <- function(var, iu, design){

  #==================================================================================#
  # Step 1 - Do argument checking -----------------------------------------
  #==================================================================================#

  # Do argument checking
  evaluate_args(var = var, iu = iu, design = design)
  evaluate_drug_name(var = var, df = design$variables)
  # Last evaluates and creates object survey_groups
  survey_groups <- evaluate_df(df = design$variables)

  #==================================================================================#
  # Step 2 - Convert argument into formulas for svy functions -------------
  #==================================================================================#

  formula_var <- stats::as.formula(paste0("~", var))
  formula_iu <- stats::as.formula(paste0("~", iu))

  formula_iu_sex <- stats::as.formula(paste0("~", iu, "+ ind_sex"))
  formula_iu_att <- stats::as.formula(paste0("~", iu, "+ ind_child_attendance"))

  drug_name <- toupper(gsub('^(ind_)(.+)(_{1})(.+)(_bin)$',"\\4", var))

  sex <- rlang::sym("sex")
  attendance <- rlang::sym("attendance")
  drug <- rlang::sym("drug")

  #==================================================================================#
  # Step 3 - Go through groups --------------------------------------------
  #==================================================================================#

  # Go through each group. If one is missing it is skipped.

  #==================================================================================#
  # Step 3.1 - Go through SAC --------------------------------------------
  #==================================================================================#

  if (1 %in% survey_groups){

    # For SAC, calculate overall result, by sex, and by attendance, give uniformity
    sac_all <- survey::svyby(formula_var, formula_iu, design = subset(design, ind_group == 1),
                             survey::svyciprop, vartype = "ci", method = "logit", na.rm = T) %>%
      tibble::remove_rownames() %>% dplyr::mutate(!!sex := NA, !! attendance := NA, !!drug := drug_name, by = "overall") %>%
      dplyr::select(area = 1, drug, sex, attendance, by, estimate = 2, lower = 3, upper = 4)


    sac_sex <- survey::svyby(formula_var, formula_iu_sex, design = subset(design, ind_group == 1),
                             survey::svyciprop, vartype = "ci", method = "logit", na.rm = T) %>%
      tibble::remove_rownames() %>% dplyr::mutate(!!attendance := NA, !!drug := drug_name, by = "sex") %>%
      dplyr::select(area = 1, drug, sex = 2, attendance, by, estimate = 3, lower = 4, upper = 5)

    sac_att <- survey::svyby(formula_var, formula_iu_att, design = subset(design, ind_group == 1),
                             survey::svyciprop, vartype = "ci", method = "logit", na.rm = T) %>%
      tibble::remove_rownames() %>% dplyr::mutate(!!sex := NA, !!drug := drug_name, by = "attendance") %>%
      dplyr::select(area = 1, drug, sex, attendance = 2, by, estimate = 3, lower = 4, upper = 5)

    sac <- rbind(sac_all, sac_sex, sac_att) %>% dplyr::mutate(group = "SAC") %>% dplyr::select(1,9,2:8)

  }

  #==================================================================================#
  # Step 3.2 - Go through Adults --------------------------------------------
  #==================================================================================#

  if (2 %in% survey_groups){

    # For adults, calculate overall result and sex, give uniformity
    adu_all <- survey::svyby(formula_var, formula_iu, design = subset(design, ind_group == 2),
                             survey::svyciprop, vartype = "ci", method = "logit", na.rm = T) %>%
      tibble::remove_rownames() %>% dplyr::mutate(!!sex := NA, !!attendance := NA, !!drug := drug_name, by = "overall") %>%
      dplyr::select(area = 1, drug, sex, attendance, by, estimate = 2, lower = 3, upper = 4)


    adu_sex <- survey::svyby(formula_var, formula_iu_sex, design = subset(design, ind_group == 2),
                             survey::svyciprop, vartype = "ci", method = "logit", na.rm = T) %>%
      tibble::remove_rownames() %>% dplyr::mutate(!!attendance := NA, !!drug := drug_name, by = "sex") %>%
      dplyr::select(area = 1, drug, sex = 2, attendance, by, estimate = 3, lower = 4, upper = 5)

    adu <- rbind(adu_all, adu_sex) %>% dplyr::mutate(group = "adults") %>% dplyr::select(1,9,2:8)

  }

  #==================================================================================#
  # Step 4 - Bind and output --------------------------------------------
  #==================================================================================#

  if (all(1:2 %in% survey_groups)){ # Both present

    result <- rbind(sac, adu)

  } else if (1 %in% survey_groups) { # SAC only

    result <- sac

  } else { # Adults only

    result <- adu

  }

  return(result)

}


#==================================================================================#
#==================================================================================#
# Additional functions (used by large function, not exported) -------------
#==================================================================================#
#==================================================================================#

#==================================================================================#
# Add - Check the variable containing drug offering or taking conforms to data dictionary -----
#==================================================================================#

evaluate_drug_name <- function(var, df){

  if (! grepl('^(ind_)(.+)(_{1})(.+)(_bin)$', var)) {

    stop("Function based on the premise that variables of interest take the form ind_[string]_[3_letter_drug_abbreviation]_bin")

  }

  values <- stats::na.omit(df[[var]])

  if ((dplyr::n_distinct(values) > 2 | dplyr::n_distinct(values) < 1) | (max(values) > 1 | min(values) < 0)) {

    stop(sprintf("The answer codes should only be 0, 1 or NA. This is not the case for %s, where the values are %s",
                 var, paste(sort(unique(values)), collapse = ", ") ))

  }
}

#==================================================================================#
# Add - Check vars in data frame -----
#==================================================================================#

# Check the variables in the data frame used to group (SAC or adult) and disaggregate (sex, school attendance)
# conform to the data dictionary
evaluate_df <- function(df){

  # Check whether data contains variable informing of group (SAC & Adults)
  # Currently the data dicitonary has it as var "ind_group", as this changes, this function would need to change
  if (!("ind_group" %in% names(df))) stop("Function requires the variable ind_group denoting group (e.g., SAC, Adult)")

  # Further, the variable ind_group is written as code 1 for SAC, code 2 for adults. Check at least one of these is contained
  # As the data dictionary evolves, this would need to reflect those changes
  survey_groups <- unique(stats::na.omit(df$ind_group))
  if (!any(1:2 %in% survey_groups)) {
    stop(sprintf("The function assumes var ind_group is coded as 1 for SAC, 2 for adults. Currently, var ind_group has answers: %s",
                 paste(survey_groups, collapse = ", ")))
  }

  # Check for sex and school attendance variables
  if (!("ind_sex" %in% names(df))) stop("Function requires the variable ind_sex denoting sex")
  if (!("ind_child_attendance" %in% names(df))) stop("Function requires the variable ind_child_attendance denoting attendance")

  if (!any(0:1 %in% unique(stats::na.omit(df$ind_sex)))) {
    stop(sprintf("The function assumes var ind_sex is coded as 0 for males, 1 for females. Currently, var ind_sex has answers: %s",
                 paste(unique(stats::na.omit(df$ind_sex)), collapse = ", ")))
  }

  if (!any(0:1 %in% unique(stats::na.omit(df$ind_child_attendance)))) {
    stop(sprintf("The function assumes var ind_child_attendance is coded as 0 for males, 1 for females. Currently, var ind_child_attendance has answers: %s",
                 paste(unique(stats::na.omit(df$ind_child_attendance)), collapse = ", ")))
  }

  return(survey_groups)

}

#==================================================================================#
# Add - Check the arguments to the function ------
#==================================================================================#

evaluate_args <- function(var, iu, design){

  # Check var is string
  if (!(is.character(var) & length(var) == 1)) stop("The name of the variable to analyse is not a simple, length one string")

  # Check var iu string
  if (!(is.character(iu) & length(iu) == 1)) stop("The name of the variable of the IU is not a simple, length one string")

  # Check design object
  if(!(class(design)[2] == "survey.design")) stop("Your design object is not a survey package design object")

}

