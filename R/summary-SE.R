#' Helper function for adding confidence intervals to stripcharts
#'
#' This function allows you to generate error bars to add to plots.
#' It is based on code from http://www.cookbook-r.com/Manipulating_data/Summarizing_data/
#' The use of the function for interaction plots is discussed at http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/
#' Function is provided here to facilitate various plots that include confidence intervals in ggplot2
#'
#' @param data a required data frame containing the variables for the plot.
#' @param measurevar the name of a column that contains the variable to be summarized (response variable)
#' @param groupvars a vector containing names of columns that contain grouping variables
#' @param na.rm a boolean that indicates whether to ignore NA's
#' @param conf.level: the proportion confidence level of the confidence interval (default is 0.95)
#' @param .drop option for ddply
#'
#' @details Function for summarizing data to add confidence intervals to plots.
#' @examples
#' # must have the carData package installed
#' data(TitanicSurvival, package = "carData")
#' library(tidyverse)
#' library(viridis)
#' library(ggplot2)
#'
#' # The errorbars overlapped, so use position_dodge to move them horizontally
#' pd <- position_dodge(0.5)
#' summaryinfo <- summarySE(data = TitanicSurvival, measurevar = "age", groupvars = c("sex", "survived"), na.rm = T, conf.level = 0.95)
#' TitanicSurvival %>% ggplot() +
#'   geom_violin(aes(x = sex, y = age, colour = survived), position = pd, alpha = 0.3) +
#'   geom_jitter(aes(x = sex, y = age, colour = survived), position = pd, alpha = 0.3) +
#'   geom_errorbar(data = summaryinfo, aes(ymin = LL, ymax = UL, x = sex, group = survived), alpha = 1, width=.2, position=pd, col = "black") +
#'   geom_line(data = summaryinfo, aes(x = sex, y = mean, group = survived), position=pd) +
#'   geom_point(data = summaryinfo, aes(x = sex, y = mean, fill = survived), position = pd, size = 3, shape = 22, col = "black", alpha = .5) +
#'   theme_bw() +
#'   scale_color_viridis_d(end = 0.9, option = "E") +
#'   scale_fill_viridis_d(end = 0.9, option = "E")

#' @export
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.level=.95, .drop=TRUE) {
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }

  library(plyr)

  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- plyr::ddply(data, groupvars, .drop=.drop,
                       .fun = function(xx, col) {
                         c(N    = length2(xx[[col]], na.rm=na.rm),
                           mean = mean   (xx[[col]], na.rm=na.rm),
                           sd   = sd     (xx[[col]], na.rm=na.rm)
                         )
                       },
                       measurevar
  )

  datac <- datac %>% mutate(SE = sd / sqrt(N),  # Calculate standard error of the mean
                            tstar = qt(conf.level/2 + .5, N-1),
                            ME = tstar * SE, #Margin of error for mean
                            LL = mean - ME, #CI lower limit
                            UL = mean + ME) #CI upper limit

  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval:
  # e.g., if conf.level is .95, use .975 (above/below), and use df=N-1 for groupwise CI

  return(datac)
}



