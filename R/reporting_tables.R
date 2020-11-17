#' A function to calculate the p values of differences between sexes and attend/non-attend SAC.
#'
#' @param partition The variable to determine by what category the data will be split by. In quotes.
#' @param data The data from the survey. A dataframe, not in quotes.
#'
#' @return A data frame the number using the inputted data, broken up by the partition argument detailing:
#'
#' * the number of distinct segments visited,
#' * the number of individuals interviewed,
#' * the percentage ofdistinct segments, individuals interviewof containing the p values for the different differences for the chosen drug and group.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang :=
#' @export rr_summary
rr_summary <- function(partition, data){

  part_sym <- rlang::sym(partition)
  groups <- unique(data$ind_group)

  result <- list()
  for (group in groups){

    # Reduce data frame to group in question
    df <- data %>% dplyr::filter(ind_group == group)
    swallow_vars <- names(df)[grepl("^ind_swallowed_[a-zA-Z]+_bin", names(df))]
    present_vars <- unique(gsub('^(ind_)(.+)(_{1})(.+)(_bin)$',"\\4", swallow_vars))

    # Create summary dataframe (h) for each drug
    summary_output <- list()
    for (k in seq(length(swallow_vars))){

      h <- df %>% dplyr::group_by(!!part_sym) %>%
        dplyr::filter(!is.na(ind_consent_bin) & !is.na(!!rlang::sym(swallow_vars[k]))) %>%
        dplyr::summarise(n_segments = dplyr::n_distinct(segment_name),
                         n_individuals = dplyr::n_distinct(ind_code),
                         attendance_count = sum(ind_child_attend_bin, na.rm = T),
                         attendance_pct = mean(ind_child_attend_bin, na.rm = T)*100,
                         attendance_pct = ifelse(is.nan(attendance_pct), NA,
                                                 sprintf("%.1f%%", attendance_pct)),
                         girls_count = sum(ind_sex == "2_Female", na.rm = T),
                         girls_pct = sprintf("%.1f%%", (girls_count/n_individuals)*100))
      summary_output[[toupper(present_vars[k])]] <- h

    }

    # Unite the drugs under the group banner
    group_banner <- gsub('^(\\d{1})(_{1})([A-Za-z]+)$',"\\3", group)
    result[[group_banner]] <- dplyr::bind_rows(summary_output, .id = "drug") %>%
      dplyr::rename(partition = !!part_sym)

  }

  # Unite the groups, including ID column for groups
  result <- dplyr::bind_rows(result, .id = "group")
  return(result)
}


#' A function to create the RR table from the output of the other calculation functions.
#'
#' @rdname rr_summary
#' @param cs_values_output Data frame output of the `estimate_cs_values` function.
#' @param p_values_output Data frame output of the `calculate_p_values` function.
#' @param rr_summary_output Data frame output of the `rr_summary` function
#'
#' @return A data frame to b used in the SCIF RR for CES. Note that general info like % of girls in the survey
#'   is done once for every drug. Rarely is going to change, historically only one number is shown. Here kept
#'   to show if something is off. When transcribing to Word or Excel it is easy to just drop that line.
#' @export
rr_table <- function(cs_values_output, p_values_output, rr_summary_output){

  #==============================================#
  # Evaluate all have the same partition argument
  #==============================================#

  eval_equal <- all(identical(sort(unique(p_values_output$partition)), sort(unique(cs_values_output$partition))),
                    identical(sort(unique(p_values_output$partition)), sort(unique(rr_summary_output$partition))))
  if (!eval_equal) stop("The three different argument inputs do not have the same partitions, please review")

  #==============================================#
  # Transform cs_values_output (cvo)
  #==============================================#

  cvo <- list()
  # For each group
  for (group in sort(unique(cs_values_output$group), decreasing = T)){
    # Reduce df
    df <- cs_values_output[cs_values_output$group == group,]

    # Block 1 - overall results
    b1 <- df %>% dplyr::filter(by == "overall") %>%
      dplyr::mutate(item = "overall",
                    value = sprintf("%.1f%% (%.1f%%, %.1f%%)", estimate*100, lower*100, upper*100)) %>%
      dplyr::select(partition, drug, item, question, value) %>%
      tidyr::pivot_wider(names_from = partition, names_glue = "{partition}", values_from = value) %>%
      dplyr::arrange(drug, dplyr::desc(question))

    if(grepl("SAC", group)){

      # Block 2 - Attending
      b2 <- df %>% dplyr::filter(by == "attendance") %>%
        dplyr::mutate(item = ifelse(attendance == 0, "non attending", "attending"),
                      estimate = sprintf("%.1f%%", estimate*100)) %>%
        dplyr::select(partition, drug, item, question, estimate) %>%
        tidyr::pivot_wider(names_from = partition, names_glue = "{partition}", values_from = estimate) %>%
        dplyr::arrange(drug, item, dplyr::desc(question))

    }

    # Block 3 - Sex
    b3 <- df %>% dplyr::filter(by == "sex") %>%
      dplyr::mutate(item = ifelse(sex == "1_Male", "Male", "Female"), estimate = sprintf("%.1f%%", estimate*100)) %>%
      dplyr::select(partition, drug, item, question, estimate) %>%
      tidyr::pivot_wider(names_from = partition, names_glue = "{partition}", values_from = estimate) %>%
      dplyr::arrange(drug, item, dplyr::desc(question))

    if (grepl("SAC", group)){ # SAC
      cvo[[group]] <- dplyr::bind_rows(b1, b2, b3)
    } else { # Adults
      cvo[[group]] <- dplyr::bind_rows(b1, b3)
    }

  }

  cvo <- dplyr::bind_rows(cvo, .id = "group")

  #==============================================#
  # Transform p_values_output (pvo)
  #==============================================#

  pvo <- p_values_output %>%
    tidyr::pivot_wider(names_from = partition, names_glue = "{partition }", values_from = p_value) %>%
    dplyr::mutate_if(is.numeric, as.character)

  #==============================================#
  # Transform rr_summary_output (rso)
  #==============================================#

  t <- rr_summary_output %>%
    tidyr:: pivot_wider(names_from = partition, names_glue = "{partition} {.value}", values_from = n_segments:girls_pct)

  m <- list()
  for (part in unique(rr_summary_output$partition)){
    a <- paste0(part, " (.*)")
    x <- t %>% dplyr::select(group, drug, dplyr::starts_with(part)) %>% dplyr::mutate_if(is.numeric, as.character)
    if (part == unique(rr_summary_output$partition)[1]){
      m[[part]] <- x %>% tidyr::pivot_longer(dplyr::starts_with(part), names_to = c("item"), names_pattern = a, values_to = part)
    } else {
      m[[part]] <- x %>% tidyr::pivot_longer(dplyr::starts_with(part), names_to = c("item"), names_pattern = a, values_to = part) %>%
        dplyr::pull(part)
    }
  }

  rso <- dplyr::bind_cols(m)
  rso$question <- NA # Add so column is present, same place
  rso <- rso %>% dplyr::select(1:3,tidyselect::last_col(), tidyselect::everything()) # Order same as others

  #==============================================#
  # Make unified for RR
  #==============================================#

  rr_general_block <- dplyr::bind_rows(
    rso %>% dplyr::filter(item %in% c("n_segments", "n_individuals")),
    cvo %>% dplyr::filter(item == "overall"))

  rr_attend_block <- dplyr::bind_rows(
    a = rso %>% dplyr::filter(item == "attendance_pct"),
    b = cvo %>% dplyr::filter(item %in% c("attending", "non attending")),
    b = pvo %>% dplyr::filter(item == "p_diff_att"), .id = "block") %>%
    dplyr::arrange(block, drug, dplyr::desc(question)) %>% dplyr::select(-block)

  rr_sex_block <- dplyr::bind_rows(
    a = rso %>% dplyr::filter(item == "girls_pct"),
    b = cvo %>% dplyr::filter(item %in% c("Female", "Male")),
    b = pvo %>% dplyr::filter(item == "p_diff_sex"), .id = "block")  %>%
    dplyr::arrange(block, drug, dplyr::desc(question)) %>% dplyr::select(-block)

  rr_output <- dplyr::bind_rows(rr_general_block, rr_attend_block, rr_sex_block)

  #==============================================#
  # Split by groups, rejoin
  #==============================================#

  result <- list()
  for (group in sort(unique(rr_output$group), decreasing = T)){
    result[[group]] <- rr_output[rr_output$group == group,]
  }
  result <- dplyr::bind_rows(result)

  return(result)
}
