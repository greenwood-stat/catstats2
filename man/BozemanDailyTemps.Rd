% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data-BozemanDailyTemps.R
\docType{data}
\name{BozemanDailyTemps}
\alias{BozemanDailyTemps}
\title{Daily weather data from MSU-Bozeman weather station from 1893 to 2022}
\format{
a \code{data.frame} with 46603 observations on 6 variables:
\describe{
\item{PRCP}{Precipitation in inches}
\item{SNOW}{Snowfall in inches}
\item{SNWD}{Snow depth in inches}
\item{TMAX}{Daily maximum temperature in degrees F}
\item{TMIN}{Daily minimum temperature in degrees F}
}
}
\source{
http://www.wcc.nrcs.usda.gov/snotel/Montana/montana.html
}
\usage{
BozemanDailyTemps
}
\description{
Downloaded and then wrangled daily weather data to get to aggregated monthly and yearly data.
}
\details{
Location information: GHCND:USC00241044 Latitude/Longitude	45.6621°, -111.0463° Elevation	1497.5 m

Results were in "standard" (so non-metric) units

PRCP = Precipitation (inches)
SNOW = Snowfall (inches)
SNWD = Snow depth (inches)
TMAX = Maximum temperature (degrees F)
TMIN = Minimum temperature (degrees F)
}
\examples{
library(tidyverse)
BozemanDailyTemps <- BozemanDailyTemps \%>\% mutate(
  Year = year(DATE_correct),
  Month = month(DATE_correct),
  Year.Month = interaction(Year, Month))

  BozemanDailyTemps_Monthlyagg <- BozemanDailyTemps \%>\%
  group_by(Year, Month) \%>\%
  summarise(MonthlyMeanMaxTemp = mean(TMAX),
              MonthlyMeanMinTemp = mean(TMIN),
              TotalMonthlyPrecip = sum(PRCP),
              TotalMonthlySnow = sum(SNOW),
              DaysMinBelow0 = sum(TMIN < 0),
              DaysMinBelow32 = sum(TMIN < 32),
              DaysMinOver50 = sum(TMIN > 50),
              DaysMaxBelow0 = sum(TMAX < 0),
              DaysMaxBelow32 = sum(TMAX < 32),
              DaysMaxOver50 = sum(TMAX > 50),
              DaysMaxOver80 = sum(TMAX > 80),
              CountofDays = n()
              )
BozemanMonthlyAgg <- BozemanDailyTemps_Monthlyagg \%>\% filter(CountofDays > 20) #Only keep months with more than 20 observations available
BozemanYearlyAgg <- BozemanMonthlyAgg \%>\%
  group_by(Year) \%>\%
  summarise(YearlyMeanMaxTemp = mean(MonthlyMeanMaxTemp),
              YearlyMeanMinTemp = mean(MonthlyMeanMinTemp),
              TotalYearlyPrecip = sum(TotalMonthlyPrecip),
              TotalYearlySnow = sum(TotalMonthlySnow),
              DaysMinBelow0 = sum(DaysMinBelow0),
              DaysMinBelow32 = sum(DaysMinBelow32),
              DaysMinOver50 = sum(DaysMinOver50),
              DaysMaxBelow0 = sum(DaysMaxBelow0),
              DaysMaxBelow32 = sum(DaysMaxBelow32),
              DaysMaxOver50 = sum(DaysMaxOver50),
              DaysMaxOver80 = sum(DaysMaxOver80),
              CountofDays = sum(CountofDays),
              CountofMonths = n())
 BozemanYearlyAggR <- BozemanYearlyAgg \%>\% dplyr::filter(CountofMonths == 12)

 p1 <- BozemanYearlyAggR \%>\% drop_na(YearlyMeanMaxTemp) \%>\%
 ggplot(mapping = aes(x = Year, y = YearlyMeanMaxTemp)) +
 geom_point(aes(color = YearlyMeanMinTemp), alpha = 0.8) +
 geom_smooth(method = "lm") +
 geom_smooth(lty = 2, col = "red", linewidth = 1.5, se = F) + #Add smoothing line
 theme_bw() +
 scale_color_viridis_c(end = 0.9, option = "A", direction = -1) +
 labs(title = "Scatterplot of Bozeman Yearly Average Max Temperatures",
  y = "Mean Maximum Temperature (degrees F)")

p2 <- BozemanYearlyAggR \%>\% drop_na(YearlyMeanMinTemp) \%>\%
ggplot(mapping = aes(x = Year, y = YearlyMeanMinTemp)) +
geom_point(aes(color = YearlyMeanMaxTemp), alpha = 0.8) +
geom_smooth(method = "lm") +
geom_smooth(lty = 2, col = "red", linewidth = 1.5, se = F) + #Add smoothing line
theme_bw() +
scale_color_viridis_c(end = 0.9, option = "B", direction = -1) +
labs(title = "Scatterplot of Bozeman Yearly Average Min Temperatures",
    y = "Mean Minimum Temperature (degrees F)")

library(patchwork)
p1 / p2

library(plotly)
#ggplotly(p1) #not run
#ggplotly(p2) #not run

m_max <- lm(YearlyMeanMaxTemp ~ Year,
data = BozemanYearlyAggR \%>\% drop_na(YearlyMeanMaxTemp))
par(mfrow = c(2,2))
plot(m_max, pch = 16)
plot(m_max, which = 4)
}
\references{
Greenwood, M. (2022) Intermediate Statistics with R, Version 3.1. https://doi.org/10.15788/2019.08.19
}
\keyword{data}
\keyword{elevation}
\keyword{snow}
\keyword{temperature}
