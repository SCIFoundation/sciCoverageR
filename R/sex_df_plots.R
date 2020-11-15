#' CES sex plot
#'
#' A function to make the plot dissagregated by sex for dashboards from the `estimate_cs_values` output.
#'
#' @param survey_estimates A data frame containing the output from the `estimate_cs_values` function.
#' @param target A numeric value between 0 and 1 to set the WHO target coverage. Default is 0.75.
#' @param target_text A string denoting the text that describes the target. Default is `WHO\nminimun target`.
#'   Note the `\n` serves to make a new line in the legend text (see output).
#' @param theme_object A ggplot theme object (e.g. theme_classic()) for the plots. Defaults is NA.
#'   In that case, the function calls `make_sci_theme()` to create the default theme. See that function for theme details.
#' @param y_axis_label_reach Y axis label for programme reach. Default is `Programme Reach (in %)\n"`.
#'   Note the `\n` is included to create a blank line underneath to make space. Same for other axis labels.
#' @param y_axis_label_coverage Y axis label for survey coverage. Default is `Survey Coverage (in %)\n`.
#' @param x_axis_label X axis label denoting the partitions. Default is `Implementation Unit`.
#' @param sex_group_names A string of lenght four giving the names to be shown in the X axis for (in this order and
#'   always four even if fewer cases in the `survey estimates` df) Male SAC, Male Adults, Female SAC, and Female Adults.
#'   Default is English: `Boys`, `Men`, `Girls`, and `Women`.
#' @param survey_point_shape Shape of the survey data point. Default is a filled dot (16).
#' @param survey_point_size Size of the survey data point. Default is 4.
#' @param nrow_guide Number of rows the partition levels shall be shown in. Default is `NA` which will set it to
#'   next integer of the division (number of partition levels / 2) (e.g., if 5 levels, nrow_guide is 3).
#'
#' @return A list of plots. Run names({make_overall_plot output}) to see them. Call them individually by using
#'   `{make_overall_plot output}[[{name of the particular plot}]]`.
#'
#' @importFrom magrittr %>%
#' @export
make_sex_plot <- function(survey_estimates, target = 0.75,
                          target_text = "WHO\nminimum target",
                          theme_object = NA,
                          y_axis_label_reach = "Programme Reach (in %)\n",
                          y_axis_label_coverage = "Survey Coverage (in %)\n",
                          x_axis_label = "\nImplementation Unit",
                          sex_group_names = c("Boys", "Men", "Girls", "Women"),
                          survey_point_shape = 16, survey_point_size = 4,
                          nrow_guide = NA){

  #==========================================================================#
  # Step 1 - Reduce survey estimates to sex disaggregation ------------------
  #==========================================================================#

  data <- survey_estimates[survey_estimates$by == "sex", ]

  #==========================================================================#
  # Step 2 - Evaluate sex_group_names ----------------------------
  #==========================================================================#

  eval_sex_group_names(sex_group_names)

  #==========================================================================#
  # Step 3 - Create additional objects --------------------------------------
  #==========================================================================#

  # Create DF for horizontal target
  if (!is.na(target)) target_df <- data.frame(x=c(-Inf, Inf),y=target,z=sprintf(paste0("%s%% " , target_text),round(target*100, 0)))

  # Create theme_object if none passed
  if (all(is.na(theme_object))) theme_object <- make_sci_theme()

  # Make sex a factor
  data$sex_factor <- dplyr::case_when(data$sex == "1_Male" & data$group == "SAC" ~ sex_group_names[1],
                                      data$sex == "1_Male" & data$group == "Adult" ~ sex_group_names[2],
                                      data$sex == "2_Female" & data$group == "SAC" ~ sex_group_names[3],
                                      data$sex == "2_Female" & data$group == "Adult" ~ sex_group_names[4])
  data$sex_factor <- factor(data$sex_factor)

  # Make partition factor
  data$partition_factor <- factor(data$partition)

  #==========================================================================#
  # Step 4 - Create plots looping through groups, question types in groups -
  #==========================================================================#

  plots <- list()
  for (group in (unique(data$group))){

    # Reduce DF to relevant group
    df <- data[data$group == group,]

    # Colours done here in case one group had more IUs than another (e.g., adult survey in fewer IUs)
    num_ius <- dplyr::n_distinct(df$partition, na.rm = TRUE)
    colours <- sciColours::sci_pal(palette= "secondary")(num_ius)

    # Add nrow for guide if missing (done here because depends on num_ius)
    if (is.na(nrow_guide)) nrow_guide <- round(num_ius / 2, 0)

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
        ggplot2::geom_point(data = df_q, ggplot2::aes(x = sex_factor, y = estimate, col = partition_factor),
                            shape = survey_point_shape, size = survey_point_size) +
        ggplot2::geom_line(data = df_q, ggplot2::aes(x = sex_factor, y = estimate, col = partition_factor,
                                                     group = partition_factor), size = 1) +

        ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits= c(0, 1.05),
                                    breaks = seq(0, 1, 0.25)) +
        ggplot2::labs(y = y_axis_label, x = paste0("\n", x_axis_label), title = "") +
        ggplot2::scale_colour_manual(values = sciColours::sci_pal(palette= "secondary")(num_ius)) +
        theme_object + ggplot2::facet_wrap(~drug, scales = "free_x") +
        ggplot2::guides(colour = ggplot2::guide_legend(nrow = nrow_guide, byrow = F)) +
        ggplot2::theme(legend.spacing.x = ggplot2::unit(0.10, 'cm'))

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
# Evaluate sex group names
#=============================================================================#

eval_sex_group_names <- function(x){

  if (!is.character(x)) stop("You need to define the sex groups as a string")
  if (!(length(x) == 4)) stop(sprintf("The string defining the sex groups needs to be of length 4, to cover Male SAC, Male Adults, Female SAC, and Female Adults. Even if you data does not have all these cases, you need to pass them if you do not want to use the default. Your current input is %s", x))

}
