#' Interaction Plot array with confidence intervals with ggplot
#'
#' This function allows you to generate interaction plots for a Two-Way ANOVA
#' with error bars with an array of plots and main effect type plots with faceting.

#' @param response the name of a column that contains the variable to be summarized (response variable)
#' @param groupvars a vector containing names of columns that contain grouping variables
#' @param na.rm a boolean that indicates whether to ignore NA's
#' @param conf.level: the proportion confidence level of the confidence interval (default is 0.95)
#' @param pd position dodge to shift groups (default is 0.1)
#' @param ptalpha alpha value for the points, set to 0 to hide points and just view means, CIs, and violins
#' @param ylim_manual vector of lower and upper y-axis limits for all plots
#'
#' @details Function for making nice looking interaction plot (both versions) with observations and 95% confidence intervals for each combination of groups.
#' @examples
#' # must have the carData package installed
#' data(TitanicSurvival, package = "carData")
#' intplot_gg(response = "age", groupvars = c("sex", "survived"), data = TitanicSurvival, na.rm = T, conf.level = 0.9, array = T)
#' intplot_gg(response = "age", groupvars = c("sex", "survived"), data = TitanicSurvival, na.rm = T, conf.level = 0.9, array = F)
#' intplot_gg(response = "age", groupvars = c("sex", "survived", "passengerClass"), data = TitanicSurvival, na.rm = T, conf.level = 0.9, array = F)
#' intplot_gg(response = "age", groupvars = c("sex", "survived", "passengerClass"), data = TitanicSurvival, na.rm = T, conf.level = 0.9, array = T, ptalpha = .5, pd = .5)
#' intplot_gg(data = TitanicSurvival, response = "age", groupvars = "sex", na.rm = T, conf.level = 0.95)
#' data(ToothGrowth) #Quantitative predictor with 3 levels, converted to factor
#' ToothGrowth %>% intplot_gg(response = "len", groupvars = c("dose", "supp"), na.rm = T, conf.level = 0.95, array = T, ptalpha = 0.1)

#' @export
intplot_gg <- function (data = NULL, response = NULL, groupvars = NULL, na.rm=FALSE,
                        conf.level=.95, .drop=TRUE, pd = 0.1, array = T, ptalpha = 0.3, ylim_manual = NULL)
{
  #Function to generate a plot array that have pirateplot-ish plots in off diagonals with single explanatory variables
  #Diagonals contain the intplots made both ways... and return as ggplot

  #Functionality to create interaction plots with faceting for >2 grouping variables

  #Requires summarySE function also available in package for finding means and confidence interval limits

  if(!require("tidyverse")) stop("you need to install tidyverse")
  if(!require("ggplot2")) stop("you need to install ggplot2")
  if(!require("viridis")) stop("you need to install viridis")
  if(!require("patchwork")) stop("you need to install patchwork")
  if(!require("ggpubr")) stop("you need to install ggpubr")

  data <- as.data.frame(data)

  pd <- position_dodge(pd)

  numberXs <- length(groupvars)

  #Convert all X's to factors for plotting:
  colforfac <- numeric(0)
  for (j in 1:numberXs){
    colforfac[j] <- which(names(data) == groupvars[j])
    data[,colforfac[j]] <- factor(data[,colforfac[j]])
  }

  dim1 <- dim(data)[1]
  colforresp <- which(names(data) == response)
  data <- na.omit(data[c(colforfac, colforresp)])

  colforresp <- which(names(data) == response)

  if(is.null(ylim_manual)){ylim_manual <- c(min(data[,colforresp]), max(data[,colforresp]))}

  if(dim(data)[1] < dim1){
    print(paste(dim1 - dim(data)[1], "missing observations present on variables used, removed from all plots"))
  }

  summaryinfo <- summarySE(data = data, measurevar = response, groupvars = groupvars, na.rm = na.rm, conf.level = conf.level)

  if(min(summaryinfo$N)<20 & min(summaryinfo$N) >= 3){
    print(paste0("Warning, one or more of the combinations of factor variables has a small sample size, shapes of violins for small group sizes can be misleading. The least frequently observed combination has only ", min(summaryinfo$N), " observations." ))
  }

  if(min(summaryinfo$N)<3){
    print(paste0("Warning, one or more of the combinations of factor variables has an extremely small sample size. The least frequently observed level has only ", min(summaryinfo$N), " observations. You likely have insufficient sample size to do reliable inference at these combinations." ))
  }


  if(numberXs == 1){
    print("This is not an interaction plot. Plot versus single X produced.")

    plot1 <- data %>% ggplot() +
      geom_violin(aes(x = .data[[groupvars[1]]], y = .data[[response]], color = .data[[groupvars[1]]]), alpha = 0.3) +
      geom_jitter(aes(x = .data[[groupvars[1]]], y = .data[[response]], color = .data[[groupvars[1]]]), alpha = ptalpha) +
      geom_errorbar(data = summaryinfo, aes(ymin = LL, ymax = UL, x = .data[[groupvars[1]]]), alpha = 1, width=.2, col = "black") +
      #geom_line(data = summaryinfo, aes(x = .data[[groupvars[1]]], y = mean)) +
      geom_point(data = summaryinfo, aes(x = .data[[groupvars[1]]], y = mean), size = 3, shape = 20, col = "black", alpha = .5) +
      theme_bw() +
      scale_color_viridis_d(end = 0.9, option = "E") +
      scale_fill_viridis_d(end = 0.9, option = "E") +
      ylim(ylim_manual[1], ylim_manual[2])

  }

  if(numberXs == 2 & array == T){

    plotUL <- data %>% ggplot() +
      geom_violin(aes(x = .data[[groupvars[1]]], y = .data[[response]], color = .data[[groupvars[2]]]), alpha = 0.3, position = pd) +
      geom_jitter(aes(x = .data[[groupvars[1]]], y = .data[[response]], color = .data[[groupvars[2]]]), alpha = ptalpha, position = pd) +
      geom_errorbar(data = summaryinfo, aes(ymin = LL, ymax = UL, x = .data[[groupvars[1]]], group = .data[[groupvars[2]]]), position = pd, alpha = 1, width=.2, col = "black") +
      geom_line(data = summaryinfo, aes(x = .data[[groupvars[1]]], y = mean, group = .data[[groupvars[2]]], color = .data[[groupvars[2]]]), position = pd) +
      geom_point(data = summaryinfo, aes(x = .data[[groupvars[1]]], y = mean, group = .data[[groupvars[2]]], fill = .data[[groupvars[2]]]), position = pd, size = 3, shape = 23, col = "black", alpha = 1) +
      theme_bw() +
      scale_color_viridis_d(end = 0.9, option = "C") +
      scale_fill_viridis_d(end = 0.9, option = "C") +
      ylim(ylim_manual[1], ylim_manual[2])

    summaryinfo_LL <- summarySE(data = data, measurevar = response, groupvars = groupvars[[1]], na.rm = na.rm, conf.level = conf.level)


    plotLL <- data %>% ggplot() +
      geom_violin(aes(x = .data[[groupvars[1]]], y = .data[[response]], color = .data[[groupvars[1]]]), alpha = 0.3) +
      geom_jitter(aes(x = .data[[groupvars[1]]], y = .data[[response]], color = .data[[groupvars[1]]]), alpha = ptalpha) +
      geom_errorbar(data = summaryinfo_LL, aes(ymin = LL, ymax = UL, x = .data[[groupvars[1]]]), alpha = 1, width=.2, col = "black") +
      #geom_line(data = summaryinfo, aes(x = .data[[groupvars[1]]], y = mean)) +
      geom_point(data = summaryinfo_LL, aes(x = .data[[groupvars[1]]], y = mean), size = 3, shape = 20, col = "black", alpha = .5) +
      theme_bw() +
      scale_color_viridis_d(end = 0.7, option = "A", direction = -1) +
      scale_fill_viridis_d(end = 0.7, option = "A", direction = -1) +
      ylim(ylim_manual[1], ylim_manual[2])

    plotLR <- data %>% ggplot() +
      geom_violin(aes(x = .data[[groupvars[2]]], y = .data[[response]], color = .data[[groupvars[1]]]), alpha = 0.3, position = pd) +
      geom_jitter(aes(x = .data[[groupvars[2]]], y = .data[[response]], color = .data[[groupvars[1]]]), alpha = ptalpha, position = pd) +
      geom_errorbar(data = summaryinfo, aes(ymin = LL, ymax = UL, x = .data[[groupvars[2]]], group = .data[[groupvars[1]]]), position = pd, alpha = 1, width=.2, col = "black") +
      geom_line(data = summaryinfo, aes(x = .data[[groupvars[2]]], y = mean, group = .data[[groupvars[1]]], color = .data[[groupvars[1]]]), position = pd) +
      geom_point(data = summaryinfo, aes(x = .data[[groupvars[2]]], y = mean, group = .data[[groupvars[1]]], fill = .data[[groupvars[1]]]), position = pd, size = 3, shape = 23, col = "black", alpha = 1) +
      theme_bw() +
      scale_color_viridis_d(end = 0.7, option = "A", direction = -1) +
      scale_fill_viridis_d(end = 0.7, option = "A", direction = 1) +
      ylim(ylim_manual[1], ylim_manual[2])

    summaryinfo_UR <- summarySE(data = data, measurevar = response, groupvars = groupvars[[2]], na.rm = na.rm, conf.level = conf.level)

    plotUR <- data %>% ggplot() +
      geom_violin(aes(x = .data[[groupvars[2]]], y = .data[[response]], color = .data[[groupvars[2]]]), alpha = 0.3) +
      geom_jitter(aes(x = .data[[groupvars[2]]], y = .data[[response]], color = .data[[groupvars[2]]]), alpha = ptalpha) +
      geom_errorbar(data = summaryinfo_UR, aes(ymin = LL, ymax = UL, x = .data[[groupvars[2]]]), alpha = 1, width=.2, col = "black") +
      #geom_line(data = summaryinfo, aes(x = .data[[groupvars[1]]], y = mean)) +
      geom_point(data = summaryinfo_UR, aes(x = .data[[groupvars[2]]], y = mean), size = 3, shape = 20, col = "black", alpha = .5) +
      theme_bw() +
      scale_color_viridis_d(end = 0.9, option = "C") +
      scale_fill_viridis_d(end = 0.9, option = "C") +
      ylim(ylim_manual[1], ylim_manual[2])


    plot1 <- ggarrange(plotUL, plotUR, plotLL, plotLR, nrow = 2, ncol = 2, align = "v")
    plot1 <- annotate_figure(plot1,
                             top = text_grob(paste0("Two-way interaction of ", groupvars[[1]], " and ", groupvars[[2]])))

  }

  if(numberXs == 2 & array == F){

    plot1 <- data %>% ggplot() +
      geom_violin(aes(x = .data[[groupvars[1]]], y = .data[[response]], color = .data[[groupvars[2]]]), alpha = 0.3, position = pd) +
      geom_jitter(aes(x = .data[[groupvars[1]]], y = .data[[response]], color = .data[[groupvars[2]]]), alpha = ptalpha, position = pd) +
      geom_errorbar(data = summaryinfo, aes(ymin = LL, ymax = UL, x = .data[[groupvars[1]]], group = .data[[groupvars[2]]]), position = pd, alpha = 1, width=.2, col = "black") +
      geom_line(data = summaryinfo, aes(x = .data[[groupvars[1]]], y = mean, group = .data[[groupvars[2]]], color = .data[[groupvars[2]]]), position = pd) +
      geom_point(data = summaryinfo, aes(x = .data[[groupvars[1]]], y = mean, group = .data[[groupvars[2]]], fill = .data[[groupvars[2]]]), position = pd, size = 3, shape = 23, col = "black", alpha = 1) +
      theme_bw() +
      scale_color_viridis_d(end = 0.9, option = "E") +
      scale_fill_viridis_d(end = 0.9, option = "E") +
      labs(title = paste0("Two-way interaction of ", groupvars[[1]], " and ", groupvars[[2]])) +
      ylim(ylim_manual[1], ylim_manual[2])


  }


  if(numberXs == 3 & array == T){
    #All two-way interactions but not 3-way interaction, single x on diag

    #data$allcolors <- factor(paste0(data[,colforfac[1]], data[,colforfac[2]], data[,colforfac[3]]))
    summaryinfo_11 <- summarySE(data = data, measurevar = response, groupvars = groupvars[[1]], na.rm = na.rm, conf.level = conf.level)
    summaryinfo_22 <- summarySE(data = data, measurevar = response, groupvars = groupvars[[2]], na.rm = na.rm, conf.level = conf.level)
    summaryinfo_33 <- summarySE(data = data, measurevar = response, groupvars = groupvars[[3]], na.rm = na.rm, conf.level = conf.level)
    summaryinfo_12 <- summarySE(data = data, measurevar = response, groupvars = groupvars[1:2], na.rm = na.rm, conf.level = conf.level)
    summaryinfo_13 <- summarySE(data = data, measurevar = response, groupvars = groupvars[c(1,3)], na.rm = na.rm, conf.level = conf.level)
    summaryinfo_23 <- summarySE(data = data, measurevar = response, groupvars = groupvars[2:3], na.rm = na.rm, conf.level = conf.level)

    plot11 <- data %>% ggplot() +
      geom_violin(aes(x = .data[[groupvars[1]]], y = .data[[response]], color = .data[[groupvars[1]]]), alpha = 0.3) +
      geom_jitter(aes(x = .data[[groupvars[1]]], y = .data[[response]], color = .data[[groupvars[1]]]), alpha = ptalpha) +
      geom_errorbar(data = summaryinfo_11, aes(ymin = LL, ymax = UL, x = .data[[groupvars[1]]]), alpha = 1, width=.2, col = "black") +
      #geom_line(data = summaryinfo, aes(x = .data[[groupvars[1]]], y = mean)) +
      geom_point(data = summaryinfo_11, aes(x = .data[[groupvars[1]]], y = mean), size = 3, shape = 20, col = "black", alpha = .5) +
      theme_bw() +
      scale_color_viridis_d(end = 0.7, option = "A", direction = -1) +
      scale_fill_viridis_d(end = 0.7, option = "A", direction = -1) +
      ylim(ylim_manual[1], ylim_manual[2])

    plot22 <- data %>% ggplot() +
      geom_violin(aes(x = .data[[groupvars[2]]], y = .data[[response]], color = .data[[groupvars[2]]]), alpha = 0.3) +
      geom_jitter(aes(x = .data[[groupvars[2]]], y = .data[[response]], color = .data[[groupvars[2]]]), alpha = ptalpha) +
      geom_errorbar(data = summaryinfo_22, aes(ymin = LL, ymax = UL, x = .data[[groupvars[2]]]), alpha = 1, width=.2, col = "black") +
      #geom_line(data = summaryinfo, aes(x = .data[[groupvars[1]]], y = mean)) +
      geom_point(data = summaryinfo_22, aes(x = .data[[groupvars[2]]], y = mean), size = 3, shape = 20, col = "black", alpha = .5) +
      theme_bw() +
      scale_color_viridis_d(end = 0.9, option = "C", direction = -1) +
      scale_fill_viridis_d(end = 0.9, option = "C", direction = -1) +
      ylim(ylim_manual[1], ylim_manual[2])


    plot33 <- data %>% ggplot() +
      geom_violin(aes(x = .data[[groupvars[3]]], y = .data[[response]], color = .data[[groupvars[3]]]), alpha = 0.3) +
      geom_jitter(aes(x = .data[[groupvars[3]]], y = .data[[response]], color = .data[[groupvars[3]]]), alpha = ptalpha) +
      geom_errorbar(data = summaryinfo_33, aes(ymin = LL, ymax = UL, x = .data[[groupvars[3]]]), alpha = 1, width=.2, col = "black") +
      #geom_line(data = summaryinfo, aes(x = .data[[groupvars[1]]], y = mean)) +
      geom_point(data = summaryinfo_33, aes(x = .data[[groupvars[3]]], y = mean), size = 3, shape = 20, col = "black", alpha = .5) +
      theme_bw() +
      scale_color_viridis_d(end = 0.7, option = "H", direction = 1) +
      scale_fill_viridis_d(end = 0.7, option = "H", direction = 1) +
      ylim(ylim_manual[1], ylim_manual[2])

    plot12 <- data %>% ggplot() +
      geom_violin(aes(x = .data[[groupvars[1]]], y = .data[[response]], color = .data[[groupvars[2]]]), alpha = 0.3, position = pd) +
      geom_jitter(aes(x = .data[[groupvars[1]]], y = .data[[response]], color = .data[[groupvars[2]]]), alpha = ptalpha, position = pd) +
      geom_errorbar(data = summaryinfo_12, aes(ymin = LL, ymax = UL, x = .data[[groupvars[1]]], group = .data[[groupvars[2]]]), position = pd, alpha = 1, width=.2, col = "black") +
      geom_line(data = summaryinfo_12, aes(x = .data[[groupvars[1]]], y = mean, group = .data[[groupvars[2]]], color = .data[[groupvars[2]]]), position = pd) +
      geom_point(data = summaryinfo_12, aes(x = .data[[groupvars[1]]], y = mean, group = .data[[groupvars[2]]], fill = .data[[groupvars[2]]]), position = pd, size = 3, shape = 23, col = "black", alpha = 1) +
      theme_bw() +
      scale_color_viridis_d(end = 0.9, option = "C", direction = -1) +
      scale_fill_viridis_d(end = 0.9, option = "C", direction = -1) +
      ylim(ylim_manual[1], ylim_manual[2])

    plot13 <- data %>% ggplot() +
      geom_violin(aes(x = .data[[groupvars[1]]], y = .data[[response]], color = .data[[groupvars[3]]]), alpha = 0.3, position = pd) +
      geom_jitter(aes(x = .data[[groupvars[1]]], y = .data[[response]], color = .data[[groupvars[3]]]), alpha = ptalpha, position = pd) +
      geom_errorbar(data = summaryinfo_13, aes(ymin = LL, ymax = UL, x = .data[[groupvars[1]]], group = .data[[groupvars[3]]]), position = pd, alpha = 1, width=.2, col = "black") +
      geom_line(data = summaryinfo_13, aes(x = .data[[groupvars[1]]], y = mean, group = .data[[groupvars[3]]], color = .data[[groupvars[3]]]), position = pd) +
      geom_point(data = summaryinfo_13, aes(x = .data[[groupvars[1]]], y = mean, group = .data[[groupvars[3]]], fill = .data[[groupvars[3]]]), position = pd, size = 3, shape = 23, col = "black", alpha = 1) +
      theme_bw() +
      scale_color_viridis_d(end = 0.7, option = "H", direction = 1) +
      scale_fill_viridis_d(end = 0.7, option = "H", direction = 1) +
      ylim(ylim_manual[1], ylim_manual[2])

    plot31 <- data %>% ggplot() +
      geom_violin(aes(x = .data[[groupvars[3]]], y = .data[[response]], color = .data[[groupvars[1]]]), alpha = 0.3, position = pd) +
      geom_jitter(aes(x = .data[[groupvars[3]]], y = .data[[response]], color = .data[[groupvars[1]]]), alpha = ptalpha, position = pd) +
      geom_errorbar(data = summaryinfo_13, aes(ymin = LL, ymax = UL, x = .data[[groupvars[3]]], group = .data[[groupvars[1]]]), position = pd, alpha = 1, width=.2, col = "black") +
      geom_line(data = summaryinfo_13, aes(x = .data[[groupvars[3]]], y = mean, group = .data[[groupvars[1]]], color = .data[[groupvars[1]]]), position = pd) +
      geom_point(data = summaryinfo_13, aes(x = .data[[groupvars[3]]], y = mean, group = .data[[groupvars[1]]], fill = .data[[groupvars[1]]]), position = pd, size = 3, shape = 23, col = "black", alpha = 1) +
      theme_bw() +
      scale_color_viridis_d(end = 0.7, option = "A", direction = -1) +
      scale_fill_viridis_d(end = 0.7, option = "A", direction = -1) +
      ylim(ylim_manual[1], ylim_manual[2])

    plot21 <- data %>% ggplot() +
      geom_violin(aes(x = .data[[groupvars[2]]], y = .data[[response]], color = .data[[groupvars[1]]]), alpha = 0.3, position = pd) +
      geom_jitter(aes(x = .data[[groupvars[2]]], y = .data[[response]], color = .data[[groupvars[1]]]), alpha = ptalpha, position = pd) +
      geom_errorbar(data = summaryinfo_12, aes(ymin = LL, ymax = UL, x = .data[[groupvars[2]]], group = .data[[groupvars[1]]]), position = pd, alpha = 1, width=.2, col = "black") +
      geom_line(data = summaryinfo_12, aes(x = .data[[groupvars[2]]], y = mean, group = .data[[groupvars[1]]], color = .data[[groupvars[1]]]), position = pd) +
      geom_point(data = summaryinfo_12, aes(x = .data[[groupvars[2]]], y = mean, group = .data[[groupvars[1]]], fill = .data[[groupvars[1]]]), position = pd, size = 3, shape = 23, col = "black", alpha = 1) +
      theme_bw() +
      scale_color_viridis_d(end = 0.7, option = "A", direction = -1) +
      scale_fill_viridis_d(end = 0.7, option = "A", direction = -1) +
      ylim(ylim_manual[1], ylim_manual[2])

    plot23 <- data %>% ggplot() +
      geom_violin(aes(x = .data[[groupvars[2]]], y = .data[[response]], color = .data[[groupvars[3]]]), alpha = 0.3, position = pd) +
      geom_jitter(aes(x = .data[[groupvars[2]]], y = .data[[response]], color = .data[[groupvars[3]]]), alpha = ptalpha, position = pd) +
      geom_errorbar(data = summaryinfo_23, aes(ymin = LL, ymax = UL, x = .data[[groupvars[2]]], group = .data[[groupvars[3]]]), position = pd, alpha = 1, width=.2, col = "black") +
      geom_line(data = summaryinfo_23, aes(x = .data[[groupvars[2]]], y = mean, group = .data[[groupvars[3]]], color = .data[[groupvars[3]]]), position = pd) +
      geom_point(data = summaryinfo_23, aes(x = .data[[groupvars[2]]], y = mean, group = .data[[groupvars[3]]], fill = .data[[groupvars[3]]]), position = pd, size = 3, shape = 23, col = "black", alpha = 1) +
      theme_bw() +
      scale_color_viridis_d(end = 0.7, option = "H", direction = 1) +
      scale_fill_viridis_d(end = 0.7, option = "H", direction = 1) +
      ylim(ylim_manual[1], ylim_manual[2])

    plot32 <- data %>% ggplot() +
      geom_violin(aes(x = .data[[groupvars[3]]], y = .data[[response]], color = .data[[groupvars[2]]]), alpha = 0.3, position = pd) +
      geom_jitter(aes(x = .data[[groupvars[3]]], y = .data[[response]], color = .data[[groupvars[2]]]), alpha = ptalpha, position = pd) +
      geom_errorbar(data = summaryinfo_23, aes(ymin = LL, ymax = UL, x = .data[[groupvars[3]]], group = .data[[groupvars[2]]]), position = pd, alpha = 1, width=.2, col = "black") +
      geom_line(data = summaryinfo_23, aes(x = .data[[groupvars[3]]], y = mean, group = .data[[groupvars[2]]], color = .data[[groupvars[2]]]), position = pd) +
      geom_point(data = summaryinfo_23, aes(x = .data[[groupvars[3]]], y = mean, group = .data[[groupvars[2]]], fill = .data[[groupvars[2]]]), position = pd, size = 3, shape = 23, col = "black", alpha = 1) +
      theme_bw() +
      scale_color_viridis_d(end = 0.9, option = "C", direction = -1) +
      scale_fill_viridis_d(end = 0.9, option = "C", direction = -1) +
      ylim(ylim_manual[1], ylim_manual[2])

    plot1 <- ggarrange(plot11, plot21, plot31, plot12,  plot22, plot32, plot13,  plot23 , plot33, nrow = 3, ncol = 3, align = "v")
    plot1 <- annotate_figure(plot1,
                             top = text_grob(paste0("All two-way interactions of ", groupvars[[1]], ", ", groupvars[[2]], ", and ", groupvars[[3]])))

  }

  if(numberXs == 3 & array == F){
    #Three-way interaction
    plot1 <- data %>% ggplot() +
      geom_violin(aes(x = .data[[groupvars[1]]], y = .data[[response]], color = .data[[groupvars[2]]]), alpha = 0.3, position = pd) +
      geom_jitter(aes(x = .data[[groupvars[1]]], y = .data[[response]], color = .data[[groupvars[2]]]), alpha = ptalpha, position = pd) +
      geom_errorbar(data = summaryinfo, aes(ymin = LL, ymax = UL, x = .data[[groupvars[1]]], group = .data[[groupvars[2]]]), position = pd, alpha = 1, width=.2, col = "black") +
      geom_line(data = summaryinfo, aes(x = .data[[groupvars[1]]], y = mean, group = .data[[groupvars[2]]], color = .data[[groupvars[2]]]), position = pd) +
      geom_point(data = summaryinfo, aes(x = .data[[groupvars[1]]], y = mean, group = .data[[groupvars[2]]], fill = .data[[groupvars[2]]]), position = pd, size = 3, shape = 23, col = "black", alpha = 1) +
      theme_bw() +
      scale_color_viridis_d(end = 0.9, option = "C") +
      scale_fill_viridis_d(end = 0.9, option = "C") +
      facet_wrap(~.data[[groupvars[3]]]) +
      labs(title = paste0("Three-way interaction of ", groupvars[[1]], ", ", groupvars[[2]], ", and ", groupvars[[3]])) +
      ylim(ylim_manual[1], ylim_manual[2])


  }

  if(numberXs >3){
    print("Currently not an option for more than one 3-way interaction")
  }


  #plot1

  return(plot1)

}
