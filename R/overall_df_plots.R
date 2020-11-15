#' CES Overall plot data frame
#'
#' A function to make a data frame to use for plotting for the overall cases (not disaggregated by sex or attend).
#'
#' @param survey_estimates A data frame containing the output from the `estimate_cs_values` function.
#' @param reported_df A data frame on containing the reported coverage data. Needs to have the following four columns:
#'
#'   * group - Denotes what group the reported data applies to (e.g., "SAC" or "Adult").
#'   * drug - Denotes the three letter abbreviation of the drug the reported data applies to (e.g., "PZQ").
#'   * partition - Denotes the partition the reported data applies to (e.g., "District X", "Commune Y").
#'   * estimate - Denotes the reported coverage data.
#'
#' @return A data frame containing the information from the survey estimates and the reported data in a
#'   format suitable to be passed to ggplot, in particular the `make_overall_plot` function.
#'
#' @importFrom magrittr %>%
#' @export
make_overall_plot_df <- function(survey_estimates, reported_df){

  #==========================================================================#
  # Step 1 - Evaluate input arguments ---------------------------------------
  #==========================================================================#

  # Evaluate reported_df has the right columns
  eval_reported_cols(reported_df)

  # Evaluate data is the same for reported and survey coverage (i.e., not that one has
  # SAC with PZQ and ALB in District A, the other data has only PZQ)
  eval_same_cases(survey_estimates, reported_df)

  # Add question (coverage) and type (reported) to reported_df
  reported_df <- add_reported_cols(reported_df)

  #==========================================================================#
  # Step 2 - Create table looping over groups in estimates, reported --------
  #==========================================================================#

  overall_plot_df <- list()
  for (group in (unique(survey_estimates$group))){

    # Extract relevant group, by category from survey_estimates, drop unnecessary columns
    df <- survey_estimates[(survey_estimates$group == group & survey_estimates$by == "overall"),
                           !names(survey_estimates) %in% c("by", "sex", "attendance")]
    df$type <- "survey"

    # Bind with reported info from relevant group, create partition factor (needed later for passing args to ggplot)
    h <- dplyr::bind_rows(df, reported_df[reported_df$group == group,])
    h$partition_factor <- factor(ifelse(is.na(h$lower), h$type, h$partition),
                                 levels = c(unique(h$partition), "reported"))

    # Save group df into list
    overall_plot_df[[group]] <- h

  }

  # Bind list into df, unify drug abbreviations
  overall_plot_df <- dplyr::bind_rows(overall_plot_df)
  overall_plot_df$drug <- toupper(overall_plot_df$drug)
  return(overall_plot_df)

}


#' CES overall plot
#'
#' A function to make the overrall plot for dashboards with the `make_overall_plot_df` output.
#'
#' @param overall_plot_df Output from the `make_overall_plot_df`
#' @param target A numerice value between 0 and 1 to set the WHO target coverage. Default is 0.75.
#' @param target_text A string denoting the text that describes the target. Default is `WHO\nminimun target`.
#'   Note the `\n` serves to make a new line in the legend text (see output).
#' @param theme_object A ggplot theme object (e.g. theme_classic()) for the plots. Defaults is NA.
#'   In that case, the function calls `make_sci_theme()` to create the default theme. See that function for theme details.
#' @param y_axis_label_reach Y axis label for programme reach. Default is `Programme Reach & 95% CI (in %)\n`.
#'   Note the `\n` is included to create a blank line underneath to make space. Same for other axis labels.
#' @param y_axis_label_coverage Y axis label for survey coverage. Default is `Survey Coverage & 95% CI (in %)\n`.
#' @param x_axis_label X axis label denoting the partitions. Default is `Implementation Unit`
#' @param survey_point_shape Shape of the survey data point. Default is a filled dot (16).
#' @param survey_point_size Size of the survey data point. Default is 4.
#' @param reported_point_shape Shape of the reported coverage data point. Default is a 5 arms star (42).
#' @param reported_point_size Size of the reported coverage data point. Default is 9.
#' @param reported_text String describing the dots for reported coverage. Default is English: `Reported coverage`.
#'
#' @return A list of plots. Run names({make_overall_plot output}) to see them. Call them individually by using
#'   `{make_overall_plot output}[[{name of the particular plot}]]`.
#'
#' @importFrom magrittr %>%
#' @rdname make_overall_plot_df
#' @export
make_overall_plot <- function(overall_plot_df, target = 0.75,
                              target_text = "WHO\nminimum target",
                              theme_object = NA,
                              y_axis_label_reach = "Programme Reach & 95% CI (in %)\n",
                              y_axis_label_coverage = "Survey Coverage & 95% CI (in %)\n",
                              x_axis_label = "\nImplementation Unit",
                              survey_point_shape = 16, survey_point_size = 4,
                              reported_point_shape = 42, reported_point_size = 9,
                              reported_text = "Reported coverage"){

  #==========================================================================#
  # Step 1 - Create additional objects --------------------------------------
  #==========================================================================#

  # Create DF for horizontal target
  if (!is.na(target)) target_df <- data.frame(x=c(-Inf, Inf),y=target,z=sprintf(paste0("%s%% " , target_text),round(target*100, 0)))

  # Create theme_object if none passed
  if (all(is.na(theme_object))) theme_object <- make_sci_theme()

  # Set shapes, sizes into a vector to pass to ggplot functions
  plot_shapes <- c(survey_point_shape, reported_point_shape)
  plot_sizes <- c(survey_point_size, reported_point_size)

  # Make type a factor
  overall_plot_df$type_factor <- factor(overall_plot_df$type, levels = c("survey", "reported"))

  #==========================================================================#
  # Step 2 - Create plots looping through groups, question types in groups -
  #==========================================================================#

  plots <- list()
  for (group in (unique(overall_plot_df$group))){

    # Reduce DF to relevant group
    df <- overall_plot_df[overall_plot_df$group == group,]

    # Colours done here in case one group had more IUs than another (e.g., adult survey in fewer IUs)
    num_ius <- dplyr::n_distinct(df$partition, na.rm = TRUE)
    colours <- c(sciColours::sci_pal(palette= "secondary")(num_ius), "black")

    # Add line type
    line_type <- "dashed"
    if (!is.na(target)) names(line_type) <- unique(target_df$z)

    # Create a plot for each question (reach, coverage)
    group_plots <- list()
    for(q in unique(df$question)){

      # Set y axis label
      y_axis_label <- ifelse(q == "reach", y_axis_label_reach, y_axis_label_coverage)

      # Reduce df to relevant question (in the relevant group)
      df_q <- df[df$question == q,]

      # Create basic plot
      plot <- ggplot2::ggplot() +
        ggplot2::geom_point(data = df_q, ggplot2::aes(x = partition, y = estimate, col = partition_factor,
                                             shape = type_factor, size = type_factor)) +
        ggplot2::geom_errorbar(data = df_q, ggplot2::aes(x = partition, ymin = lower, ymax = upper, col = partition_factor),
                               show.legend = F, width = .2) +

        ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits= c(0, 1.05),
                                    breaks = seq(0, 1, 0.25)) +
        ggplot2::labs(y = y_axis_label, x = paste0("\n", x_axis_label), title = "") +
        ggplot2::scale_size_manual(values = plot_sizes, breaks = "reported", labels = reported_text) +
        ggplot2::scale_shape_manual(values = plot_shapes, breaks = "reported", labels = reported_text) +
        theme_object + ggplot2::facet_wrap(~drug, scales = "free_x")

      # Add Colour schemes for coverage (IUs + Reported) or reach (only Reach)
      if (q == "coverage"){
        plot <- plot + ggplot2::scale_colour_manual(values = colours, breaks= NULL, labels = NULL)
      } else {
        plot <- plot + ggplot2::scale_colour_manual(values = sciColours::sci_pal(palette= "secondary")(num_ius), guide = FALSE)
      }

      # Add horizontal target line if desired
      if (!is.na(target)){
        plot <- plot +
          ggplot2::geom_hline(data = target_df, ggplot2::aes(yintercept = y, linetype = unique(z)), size = 1) +
          ggplot2::scale_linetype_manual(values = line_type)
      }

      plot_name <- paste0(group, " - ", q) # Make name "Group - Question"
      group_plots[[plot_name]] <- plot # Add to group list

    }
    plots[[group]] <- group_plots # Add group list
  }

  plots <- purrr::flatten(plots) # Design is nested (questions in groups, flatten it so it is direct)
  return(plots)

}


#=============================================================================#
# Internal Support Functions
#=============================================================================#

#=============================================================================#
# Evaluate columns of reported_df argument
#=============================================================================#
eval_reported_cols <- function(reported_df){

  expected_colnames <- c("drug", "estimate", "group", "partition")
  relevant_cols <- all(sort(names(reported_df)) == c("drug", "estimate", "group", "partition"))
  if (!relevant_cols){
    stop(sprintf("Function is predicated on the fact that the reported df has colnames called %s. The data frame provided has colnames %s",
                 paste(expected_colnames, collapse = ", "), paste(names(reported_df), collapse = ", ")))
  }
}

#=============================================================================#
# Evaluate reported, survey has same cases
#=============================================================================#
#' @import utils
utils::globalVariables("where")
eval_same_cases <- function(survey_estimates, reported_df){
  # Reduce survey estimates to only cases of coverage, since these are the ones with reported coverage
  cov_estimates <- survey_estimates[(survey_estimates$question == "coverage" & survey_estimates$by == "overall"),
                                    c("drug", "group", "partition")] %>%
    dplyr::mutate(dplyr::across(where(is.character), ~ tolower(.x))) %>%
    dplyr::arrange(dplyr::across(tidyselect::everything()))
  rep_cases <- reported_df[, c("drug", "group", "partition")] %>%
    dplyr::mutate(dplyr::across(where(is.character), ~ tolower(.x))) %>%
    dplyr::arrange(dplyr::across(tidyselect::everything()))
  eval <- all(cov_estimates == rep_cases)
  if (!eval){
    stop("The data frame of reported coverage covers different cases than those in the survey estimates data frame")
  }
}

#=============================================================================#
# Add columns to reported_df
#=============================================================================#
add_reported_cols <- function(reported_df){
  reported_df$question <- "coverage"
  reported_df$type <- "reported"
  return(reported_df)
}

#=============================================================================#
# Make SCI theme for graphs
#=============================================================================#

#' Make SCI Theme
#'
#' Function to make SCI theme for plots if none passed to `make_overall_plot`.
#'
#' @rdname make_overall_plot_df
#' @export
make_sci_theme <- function(){
  theme_object <- ggplot2::theme_set(
    ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(size = 16, face = "bold", angle = 0, vjust = 1, hjust= 0.5),
                     axis.text.y = ggplot2::element_text(size = 16, face = "bold"),
                     axis.title = ggplot2::element_text(size = 16, face = "bold"),
                     legend.position="bottom",
                     legend.title= ggplot2::element_blank(),
                     legend.text = ggplot2::element_text(size = 16, margin = ggplot2::margin(r = 10, l = 4, unit = "pt")),
                     legend.spacing.x = ggplot2::unit(1.0, 'cm'),
                     legend.box.spacing = ggplot2::unit(0.01, 'cm'),
                     legend.key = ggplot2::element_rect(fill = NA),
                     strip.text.x = ggplot2::element_text(size = 16, face = "bold"),
                     plot.title = ggplot2::element_text(size = 16, face = "bold"))
  )
  return(theme_object)
}
