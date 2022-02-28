#' CES Multiple choice Plot
#' Function to summarise and plot graph of select_multiple/tick all that apply questions
#' Creates summary data and plots at two disaggregation levels
#' Also creates a simpler graph if necessary
#'
#' @param data A data frame of multiple choice responses
#' @param var2work A response variable to plot
#' @param firstLevelDisaggregation A primary grouping variable
#' @param secondLevelDisaggregation A secondary grouping variable
#' @param thresholdInclude A number of proportion 0-1
#' @param nFacetCols Number of facet columns
#' @param splittr If TRUE, splits facet_wrap variables. Defaults to FALSE.
#' @param labels
#' @param simple If FALSE, adds geom_jitter. Defaults to FALSE
#' @param xLabel The x-axis label
#' @param yLabel The y-axis label
#' @param rotate.x.labels Rotates x-axis tick labels
#'
#' @return A list of plot and summary data
#' @export
#'
#' @examples
multipleChoicePlot <- function(data, var2work, firstLevelDisaggregation, secondLevelDisaggregation, thresholdInclude = 0,
                               nFacetCols = 3, splittr = F, labels = vector(), simple = F, xLabel = "District",
                               yLabel = "Percentage of children interviewed",
                               rotate.x.labels = 0) {

  #### Step 1 - Create data frame with disaggregation levels and dummies for options - can take multiple choice ones ####
  uniqueOptions <- unique(unlist(str_split(unique(data[[var2work]]), " ")))
  uniqueOptions <- uniqueOptions[!is.na(uniqueOptions)] # Drop NA cases

  listOptions <- list()
  for (k in uniqueOptions) listOptions[[paste0(var2work,k)]] <- as.double(grepl(sprintf("\\<%s\\>",k),data[[var2work]]))
  # sprintf and "\\<" it ensure that the whole expression are matched. Otherwise "no" matches in "no" and in "unk/no/wn"
  listDF <- bind_rows(listOptions)

  data <- cbind(data, listDF)
  data <- data[,append(c(firstLevelDisaggregation,secondLevelDisaggregation),names(listOptions))]

  #### Step 2 - Create DFs divided by first and second level disaggregation (long) and first level onlyu (long_avg) ####
  data2 <- data %>% group_by(across(all_of(c(firstLevelDisaggregation,secondLevelDisaggregation)))) %>% summarise_all(list(mean))

  data_long <- gather(data2, type, proportion, names(listOptions), factor_key=TRUE)
  data_long$type <- gsub(var2work,"",data_long$type)
  if (length(labels) ==0) data_long$type <- proper(data_long$type)

  data3 <- data %>% select(-secondLevelDisaggregation) %>% group_by(.data[[firstLevelDisaggregation]]) %>%
    summarise_all(list(mean))
  data_long_avg <- gather(data3, type, proportion, names(listOptions), factor_key=TRUE)
  data_long_avg$type <- gsub(var2work,"",data_long_avg$type)
  if (length(labels) ==0) data_long_avg$type <- proper(data_long_avg$type)

  #### Step 3 - Reduce summaries to threshold levels for graphs ####
  dfGraph <- data
  overviewDF <- dfGraph %>% select(append(firstLevelDisaggregation, names(listOptions))) %>%
    group_by(.data[[firstLevelDisaggregation]]) %>% summarise_all(list(mean))

  keeper <- names(listOptions)[map_dbl(overviewDF[,-1],max) > thresholdInclude]

  otherColumn <- keeper[grepl("_other", keeper)]
  if (length(otherColumn)>0) {
    if (length(otherColumn)>1) otherColumn <- otherColumn[!grepl("_other_",otherColumn)]
    otherPosition <- which(grepl(sprintf("\\<%s\\>",otherColumn), keeper))
    keeper <- keeper[-otherPosition]
  }

  dropper <- names(listOptions)[!names(listOptions) %in% keeper]

  if (length(dropper) >1) added <- as.double(apply(dfGraph[,dropper],1,function(x) sum(x) ))
  if (length(dropper)==1) added <- map_dbl(dfGraph[,dropper], max)
  #if (length(dropper)==0) added <- vector("numeric", dim(dfGraph)[1]) # A vector of 0s

  if (length(dropper) >0) {
    dfGraph$`999_other` <- added
    keeper <- append(keeper, "999_other")
  }
  dfGraph <- dfGraph[,append(c(firstLevelDisaggregation,secondLevelDisaggregation),keeper)]


  dfGraph2 <- dfGraph %>% group_by(across(all_of(c(firstLevelDisaggregation,secondLevelDisaggregation)))) %>%
    summarise_all(list(mean))

  summary_long <- gather(dfGraph2, type, proportion, keeper, factor_key=TRUE)
  summary_long$type <- gsub(var2work,"",summary_long$type)
  if (length(labels) ==0) summary_long$type <- proper(summary_long$type)

  if (splittr) summary_long$type <- gsub(" ","\n",summary_long$type)

  dfGraph3 <- dfGraph %>% select(-secondLevelDisaggregation) %>% group_by(.data[[firstLevelDisaggregation]]) %>%
    summarise_all(list(mean))

  summary_long_avg <- gather(dfGraph3, type, proportion, keeper, factor_key=TRUE)
  summary_long_avg$type <- gsub(var2work,"",summary_long_avg$type)
  if (length(labels) ==0) summary_long_avg$type <- proper(summary_long_avg$type)

  if (splittr) summary_long_avg$type <- gsub(" ","\n",summary_long_avg$type)

  levs <- summary_long %>% group_by(type) %>% summarise(rate = mean(proportion)) %>% arrange(-rate) %>% .$type
  summary_long$ord_type = factor(summary_long$type, levels=levs)
  summary_long_avg$ord_type = factor(summary_long_avg$type, levels=levs)

  otherString <- levs[grepl("other",tolower(levs))]
  forcats::fct_relevel(summary_long$ord_type, otherString, after = Inf)
  forcats::fct_relevel(summary_long_avg$ord_type, otherString, after = Inf)

  #### Step 4 - Grpahics ####
  if (!simple) { # Graphics either with jitter or simple

    p <- ggplot(summary_long) +
      geom_point(aes_string(x = sprintf("factor(%s)",firstLevelDisaggregation), y = "proportion",
                            col = sprintf("factor(%s)",firstLevelDisaggregation)),
                 position = position_jitter(w = 0.1, h=0.005), size = 3, alpha = 0.8) +

      geom_bar(data = summary_long_avg, aes_string(x = sprintf("factor(%s)",firstLevelDisaggregation), y = "proportion",
                                                   col = sprintf("factor(%s)",firstLevelDisaggregation),
                                                   fill = sprintf("factor(%s)",firstLevelDisaggregation)),
               stat = "identity", show.legend = T, alpha = 0.5 ) +
      labs(title = "", x = xLabel, y = yLabel)  +

      scale_y_continuous(labels = percent_format(accuracy = 1), limits=c(-0.05, 1.05)) + theme(legend.position="none")

    if (length(labels) ==0) {
      p <- p + facet_wrap(~ord_type, ncol = nFacetCols)
    } else {
      p <- p + facet_wrap(  ~ ord_type, ncol = nFacetCols, labeller=labeller(ord_type = labels))
    }


    if(rotate.x.labels != 0) p <- p + theme(axis.text.x = element_text(angle = rotate.x.labels, vjust = 1, hjust = 1))

  } else {

    p <- ggplot(summary_long_avg) +

      geom_bar(aes_string(sprintf("factor(%s)",firstLevelDisaggregation), y = "proportion",
                          fill = sprintf("factor(%s)",firstLevelDisaggregation)), stat = "identity") +
      labs(title = "", x = xLabel, y = yLabel) +

      scale_y_continuous(labels = percent_format(accuracy = 1), limits=c(0, 1)) + theme(legend.position="none")

    if (length(labels) ==0) {
      p <- p + facet_wrap(~ord_type, ncol = nFacetCols)
    } else {
      p <- p + facet_wrap(  ~ ord_type, ncol = nFacetCols, labeller=labeller(ord_type = labels))
    }
    if(rotate.x.labels > 0) p <- p + theme(axis.text.x = element_text(angle = rotate.x.labels, hjust = 1, vjust = 1))

  }
  return( list (plot = p, data.second = data_long, data.first = data_long_avg,
                plotData.first = summary_long_avg, plotData.second = summary_long))
}

#' Converting a string to first letter capitalised, all other letters little.
#'
#' This function does the same as the Excel function PROPER(string)
#'
#' @param x A character vector
#' @param type String, either 'excel' or 'cto'
#'
#' @return The same character vector but every letter at the beginning of after a blank is capitalised.
#' If type = 'cto' also the two digits and all underscores are removed
#' @examples
#' proper(c("wOrds", "many words", "verymany many Words"))
#' [1] "Words"               "Many Words"          "Verymany Many Words"
#'
#' proper(c( "01_farmer",  "02_merchant", "03_health_worker"))
#' [1] "Farmer"        "Merchant"      "Health Worker"
#'
#' @export
proper <- function(x, type = "cto") {

  if(type == "cto") {
    #x <- gsub('[0-9][0-9]_', '', x)
    x <- gsub('[0-9]+_', '', x)
    x <- gsub('_', ' ', x)
  }
  out <- gsub("(?<=\\b)([a-z])", "\\U\\1", tolower(x), perl=TRUE)
  out


} # end proper

