#' Enhanced stripchart with violins and confidence intervals
#'
#'
#' This function allows you to use a formula interface to generate a plot much like a pirateplot using ggplot2.
#'
#' It checks for missing data, converts the x variable to a factor, checks for smaller group sizes, and generates frequentist intervals for each level of the factor provided.
#'
#' @param data data frame or tibble name that contains the variables in the formula
#' @param formula a formula object such as y ~ x, where is y is quantitative and x is a variable that you want to group y based on
#' @param na.rm a boolean that indicates whether to ignore NA's
#' @param conf.level: the proportion confidence level of the confidence interval (default is 0.95)
#' @param .drop option for plyr, mostly not used
#' @param ptalpha alpha value for the points, set to 0 to hide points and just view means, CIs, and violins
#' @param jitterwidth control jitter amount in horizontal, defaults to 0.1
#' @param jitterheight control jitter amount in vertical, defaults to 0
#' @param xangle angle for text on x-axis, defaults to 45
#' @param xvjust vertical location for the x-axis text, defaults to 1
#' @param xhjust horizontal location for the x-axis text, defaults to 1
#'
#' @details Function for making enhanced stripcharts with means and confidence intervals for each combination of groups.
#' @examples
#' # must have the carData package installed
#' data(TitanicSurvival, package = "carData")
#' TitanicSurvival %>% enhanced_stripchart(age ~ passengerClass)
#' data(ToothGrowth) #Quantitative predictor with 3 levels, converted to factor
#' enhanced_stripchart(len ~ dose, data = ToothGrowth, ptalpha = 0.05,
#'   jitterwidth = 0.1, jitterheight = 0) +
#'   labs(title = "Plot of odontoblast cell lengths \n by dosage level",
#'     x = "Dosage of vitamin C",
#'     y = "Cell length (microns)")

#' @export
enhanced_stripchart <- function (data = NULL, formula = NULL, na.rm = TRUE,
                                 conf.level=.95, .drop=TRUE, ptalpha = 0.3,
                                 jitterwidth = 0.1, jitterheight = 0, ylab = NULL,
                                 xangle = 45, xvjust = 1, xhjust = 1)
{
  #Function to generate pirateplot-ish plot using ggplot

  #Requires summarySE function also available in package for finding means and confidence interval limits

  if(!require("plyr")) stop("you need to install plyr")
  if(!require("tidyverse")) stop("you need to install tidyverse")
  if(!require("ggplot2")) stop("you need to install ggplot2")
  if(!require("viridis")) stop("you need to install viridis")
  if(!require("patchwork")) stop("you need to install patchwork")
  if(!require("ggpubr")) stop("you need to install ggpubr")

  if((length(parse(text=as.character(formula[[2]]))))>1){return(print("Do not do transformations in formula call to function, transform variable prior to use of function"))}

  if(length(formula[[3]])>1){return(print("Use intplot_gg or pirateplot to visualize interactions of multiple categorical variables on the response."))}

  data <- as.data.frame(data)

  response <- eval(parse(text=as.character(formula[[2]])), data)
  respname <- formula[[2]]
  x.factor <- factor(eval(parse(text=as.character(formula[[3]])), data))
  xfname <- ((formula[[3]]))

  measurevar <- respname
  groupvars <- xfname

  #Convert X to factors for plotting:
  colforfac <- which(names(data) == groupvars)
  data[,colforfac] <- factor(data[,colforfac])

  dim1 <- dim(data)[1]
  colforresp <- which(names(data) == measurevar)
  dataR <- na.omit(data[,c(colforfac, colforresp)])

  if(dim(dataR)[1] < dim1){
    warning(paste(dim1 - dim(dataR)[1], "missing observations present on variables used, removed from all plots"))
  }

  if(min(table(dataR[,as.character(groupvars)]))<20 & min(table(dataR[,as.character(groupvars)]))>=3){
    warning(paste0("Warning, one or more of the levels of a factor variable has a small sample size, shapes of violins for small group sizes can be misleading. The least frequently observed level has only ", min(table(data[,colforfac])), " observations." ))
  }


  if(min(table(dataR[,as.character(groupvars)]))<3){
    warning(paste0("Warning, one or more of the levels of a factor variable has an extremely small sample size. The least frequently observed level has only ", min(table(data[,colforfac])), " observations. Try filtering out the least observed group and re-making the plot without that level." ))
  }

  summaryinfo <- summarySE(data = dataR, measurevar = measurevar, groupvars = groupvars, na.rm = na.rm, conf.level = conf.level)

  plot1 <- dataR %>% ggplot() +
    geom_violin(aes(x = .data[[groupvars]], y = .data[[measurevar]], color = .data[[groupvars]]), alpha = 0.3) +
    geom_jitter(aes(x = .data[[groupvars]], y = .data[[measurevar]], color = .data[[groupvars]]), alpha = ptalpha, width = jitterwidth, height = jitterheight) +
    geom_errorbar(data = summaryinfo, aes(ymin = LL, ymax = UL, x = .data[[groupvars]]), alpha = 1, width=.2, col = "black") +
    #geom_line(data = summaryinfo, aes(x = .data[[groupvars]], y = mean)) +
    geom_point(data = summaryinfo, aes(x = .data[[groupvars]], y = mean), size = 3, shape = 20, col = "black", alpha = .5) +
    theme_bw() +
    scale_color_viridis_d(end = 0.8, option = "E") +
    scale_fill_viridis_d(end = 0.8, option = "E") +
    labs(title = paste0("Enhanced stripchart of ", measurevar, " by ", groupvars)) +
    theme(axis.text.x = element_text(angle = xangle, vjust = xvjust, hjust = xhjust)) +
    theme(legend.position = "none")

  return(plot1)

}
