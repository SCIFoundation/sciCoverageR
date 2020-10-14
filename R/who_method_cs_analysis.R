#' A function to obtain binary var (e.g., coverage: yes / no) estimates with CI from WHO method CS clean data.
#'
#' @param var A string denoting the name of the variable to analyse.
#' @param part A string denoting the name of the variable to use for partitioning the data .
#' @param design A survey package design object.
#' @param level Confidence level for interval
#' @param degf Degrees of freedom. By default NA, which call uses the svyciprop default `defg(design)` evaluation (see svyciprop documentation). For user inputted degrees of freedom enter a positive integer.
#'
#' @return A data frame containing the survey estimates for the inputted variable over the inputted administrative disaggregation (commonly the implementation unit) for the survey groups (SAC and/or adults) over all individuals, disaggregated by sex and (for SAC) by school attendance.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang :=
#' @export
estimate_cs_values <- function(var, part, design, level = 0.95, degf = NA){

  #==================================================================================#
  # Step 1 - Do argument checking -----------------------------------------
  #==================================================================================#

  # Do argument checking
  evaluate_args(var = var, part = part, design = design)
  evaluate_drug_name(var = var, df = design$variables)
  # Last evaluates and creates object survey_groups
  survey_groups <- evaluate_df(df = design$variables)

  #==================================================================================#
  # Step 2 - Convert argument into formulas for svy functions -------------
  #==================================================================================#

  formula_var <- stats::as.formula(paste0("~", var))
  formula_part <- stats::as.formula(paste0("~", part))

  formula_part_sex <- stats::as.formula(paste0("~", part, "+ ind_sex"))
  formula_part_att <- stats::as.formula(paste0("~", part, "+ ind_child_attend_bin"))

  drug_name <- toupper(gsub('^(ind_)(.+)(_{1})(.+)(_bin)$',"\\4", var))

  # Convert strings to rlang sym objects to pass them thorugh piping, dplyr objects
  sex <- rlang::sym("sex")
  attendance <- rlang::sym("attendance")
  drug <- rlang::sym("drug")

  var_sym <- rlang::sym(var)
  part_sym <- rlang::sym(part)

  #==================================================================================#
  # Step 3 - Reduce design to relevant partitions ------------------------------------
  #==================================================================================#

  # Certain partitions may be available to one variable but not to another. For example, a survey may be for PZQ in 2
  # IUs, but only in one of them for ALB. In that case, all ALB var answers in the other partition are missing (NA).
  # Inversely, none is not NA. Use this to reduce the design to the relevant partitions

  relevant_partitions <- design$variables %>%
    dplyr::group_by(!!part_sym) %>% dplyr::summarise(count = sum(!is.na(!!var_sym))) %>%
    dplyr::filter(count > 0) %>% dplyr::pull(!!part_sym)

  design_f <- subset(design, eval(rlang::expr(!!part_sym)) %in% relevant_partitions)

  #==================================================================================#
  # Step 4 - Go through groups --------------------------------------------
  #==================================================================================#

  # Go through each group. If one is missing it is skipped.

  #==================================================================================#
  # Step 4.1 - Go through SAC --------------------------------------------
  #==================================================================================#

  if ("1_SAC" %in% survey_groups){

    group <- "1_SAC"
    # For SAC, calculate overall result, by sex, and by attendance, give uniformity
    sac_all <- short_svyby(formula_var, formula_part, design = design_f, group, level = level, degf = degf) %>%
      dplyr::mutate(!!sex := NA, !! attendance := NA, !!drug := drug_name, by = "overall") %>%
      dplyr::select(partition = 1, drug, by, sex, attendance, estimate = 2, lower = 3, upper = 4)


    sac_sex <- short_svyby(formula_var, formula_part_sex, design = design_f, group, level = level, degf = degf) %>%
      dplyr::mutate(!!attendance := NA, !!drug := drug_name, by = "sex") %>%
      dplyr::select(partition = 1, drug, by, sex = 2, attendance, estimate = 3, lower = 4, upper = 5)

    sac_att <- short_svyby(formula_var, formula_part_att, design = design_f, group, level = level, degf = degf) %>%
      dplyr::mutate(!!sex := NA, !!drug := drug_name, by = "attendance") %>%
      dplyr::select(partition = 1, drug, by, sex, attendance = 2, estimate = 3, lower = 4, upper = 5)

    sac <- rbind(sac_all, sac_sex, sac_att) %>% dplyr::mutate(group = "SAC") %>% dplyr::select(1,9,2:8)

  }

  #==================================================================================#
  # Step 4.2 - Go through Adults --------------------------------------------
  #==================================================================================#

  if ("2_Adult" %in% survey_groups){

    group <- "2_Adult"
    # For adults, calculate overall result and sex, give uniformity
    adu_all <- short_svyby(formula_var, formula_part, design = design_f, group, level = level, degf = degf) %>%
      dplyr::mutate(!!sex := NA, !!attendance := NA, !!drug := drug_name, by = "overall") %>%
      dplyr::select(partition = 1, drug, by, sex, attendance, estimate = 2, lower = 3, upper = 4)


    adu_sex <- short_svyby(formula_var, formula_part_sex, design = design_f, group, level = level, degf = degf) %>%
      dplyr::mutate(!!attendance := NA, !!drug := drug_name, by = "sex") %>%
      dplyr::select(partition = 1, drug, by, sex = 2, attendance, estimate = 3, lower = 4, upper = 5)

    adu <- rbind(adu_all, adu_sex) %>% dplyr::mutate(group = "adults") %>% dplyr::select(1,9,2:8)

  }

  #==================================================================================#
  # Step 5 - Bind and output --------------------------------------------
  #==================================================================================#

  if (all(c("1_SAC", "2_Adult") %in% survey_groups)){ # Both present

    result <- rbind(sac, adu)

  } else if ("1_SAC" %in% survey_groups) { # SAC only

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

  # Further, the variable ind_group is written as code << 1_SAC >> for SAC, code << 2_Adult >> for adults.
  # Check at least one of these is contained. As the data dictionary evolves, this would need to reflect those changes
  survey_groups <- unique(stats::na.omit(df$ind_group))
  if (!any(c("1_SAC", "2_Adult") %in% survey_groups)) {
    stop(sprintf("The function assumes var ind_group is coded as << 1_SAC >> for SAC, code << 2_Adult >> for adults. Currently, var ind_group has answers: %s",
                 paste(survey_groups, collapse = ", ")))
  }

  # Check for sex and school attendance variables
  if (!("ind_sex" %in% names(df))) stop("Function requires the variable ind_sex denoting sex")
  if (!("ind_child_attend_bin" %in% names(df))) stop("Function requires the variable ind_child_attend_bin denoting attendance")

  if (!any(c("1_Male", "2_Female") %in% unique(stats::na.omit(df$ind_sex)))) {
    stop(sprintf("The function assumes var ind_sex is coded as << 1_Male >> for males, << 2_Female >> for females. Currently, var ind_sex has answers: %s",
                 paste(unique(stats::na.omit(df$ind_sex)), collapse = ", ")))
  }

  if ("1_SAC" %in% survey_groups){ # Evaluate child attendance variable only if there are kids in the survey
    if (!any(0:1 %in% unique(stats::na.omit(df$ind_child_attend_bin)))) {
      stop(sprintf("The function assumes var ind_child_attend_bin is coded as 0 for males, 1 for females. Currently, var ind_child_attend_bin has answers: %s",
                   paste(unique(stats::na.omit(df$ind_child_attend_bin)), collapse = ", ")))
    }
  }

  return(survey_groups)

}

#==================================================================================#
# Add - Check the arguments to the function ------
#==================================================================================#

evaluate_args <- function(var, part, design){

  # Check design object
  if(!(sort(unique(class(design)))[1] == "survey.design")) stop("Your design object is not a survey package design object")

  # Check var is string
  if (!(is.character(var) & length(var) == 1)) stop("The name of the variable to analyse is not a simple, length one string")

  # Check var is a column in df
  if(!var %in% names(design$variables)) stop(sprintf("Variable %s is not a column header name for the design object passed", var))

  # Check part is string
  if (!(is.character(part) & length(part) == 1)) stop("The name of the variable of the partition is not a simple, length one string")

  # Check part is a column in df
  if(!part %in% names(design$variables)) stop(sprintf("Partition argument %s is not a column header name for the design object passed", part))

}

#==================================================================================#
# Add - survey shorthand ------
#==================================================================================#

short_svyby <- function(formula_var, formula_part, design, group, level = level, degf = degf) {

  if (is.na(degf)){

    survey::svyby(formula_var, formula_part, design = subset(design, ind_group == group),
                  survey::svyciprop, vartype = "ci", method = "logit", na.rm = T, level = level) %>%
      tibble::remove_rownames()

  } else {

    survey::svyby(formula_var, formula_part, design = subset(design, ind_group == group),
                  survey::svyciprop, vartype = "ci", method = "logit", na.rm = T, level = level, df = degf) %>%
      tibble::remove_rownames()

  }

}


