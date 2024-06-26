% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggintplot.R
\name{ggintplot}
\alias{ggintplot}
\title{Interaction Plot array with confidence intervals with ggplot}
\usage{
ggintplot(
  data = NULL,
  response = NULL,
  groupvars = NULL,
  na.rm = FALSE,
  conf.level = 0.95,
  .drop = TRUE,
  pd = 0.1,
  array = T,
  ptalpha = 0.3,
  ylim_manual = NULL,
  jitter.width = NULL,
  jitter.height = 0
)
}
\arguments{
\item{response}{the name of a column that contains the variable to be summarized (response variable)}

\item{groupvars}{a vector containing names of columns that contain grouping variables}

\item{na.rm}{a boolean that indicates whether to ignore NA's}

\item{pd}{position dodge to shift groups (default is 0.1)}

\item{array}{For three predictors, switches between 3-way interaction (array = F),
and main effects and all 2-way interactions (array = T)}

\item{ptalpha}{alpha value for the points, set to 0 to hide points and just view means, CIs, and violins}

\item{ylim_manual}{vector of lower and upper y-axis limits for all plots}

\item{jitter.width}{control jitter of points horizontally in the main effects, defaults to null to use geom_jitter defaults}

\item{jitter.height}{control jitter of points vertically in the main effects, defaults to 0 jitter vertically so actual values of responses are shown}

\item{conf.level:}{the proportion confidence level of the confidence interval (default is 0.95)}
}
\description{
This function allows you to generate interaction plots for a Two-Way ANOVA
with error bars with an array of plots and main effect type plots with faceting.
}
\details{
Function for making nice looking interaction plot (both versions) with observations and 95\% confidence intervals for each combination of groups.
}
\examples{
# must have the carData package installed
data(TitanicSurvival, package = "carData")
ggintplot(response = "age", groupvars = c("sex", "survived"), data = TitanicSurvival, na.rm = T, conf.level = 0.9, array = T)
ggintplot(response = "age", groupvars = c("sex", "survived"), data = TitanicSurvival, na.rm = T, conf.level = 0.9, array = F)
ggintplot(response = "age", groupvars = c("sex", "survived", "passengerClass"), data = TitanicSurvival, na.rm = T, conf.level = 0.9, array = F)
ggintplot(response = "age", groupvars = c("sex", "survived", "passengerClass"), data = TitanicSurvival, na.rm = T, conf.level = 0.9, array = T, ptalpha = .5, pd = .5)
ggintplot(data = TitanicSurvival, response = "age", groupvars = "sex", na.rm = T, conf.level = 0.95)
data(ToothGrowth) #Quantitative predictor with 3 levels, converted to factor
ToothGrowth \%>\% ggintplot(response = "len", groupvars = c("dose", "supp"), na.rm = T, conf.level = 0.95, array = T,
ptalpha = 0.1, jitter.width = .1, jitter.height = 0.1)
}
