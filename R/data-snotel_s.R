#' SNOTEL data from Montana on a day in Spring 2005
#'
#' A random sample of 25 Montana locations (from the population of 85 at the time) were obtained from the Natural Resources Conversation Service’s website (http://www.wcc.nrcs.usda.gov/snotel/Montana/montana.html). Obtained on a day in Spring 2005.
#' Information on the snow depth (Snow.Depth) in inches, daily Minimum and Maximum Temperatures (Min.Temp and Max.Temp) in F and elevation of the site (Elevation) in feet, along with the site name.
#'
#' @format a \code{data.frame} with 25 observations on 6 variables:
#' \describe{
#'   \item{ID}{Numerical ID code (not from NRCS)}
#'   \item{Station}{Name of station}
#'   \item{Snow.Depth}{Snow depth in inches}
#'   \item{Max.Temp}{Daily maximum temperature in degrees F}
#'   \item{Min.Temp}{Daily minimum temperature in degrees F}
#'   \item{Elevation}{Elevation of sampling location in feet}
#' }
#'
#' @name snotel_s
#' @docType data
#' @references Greenwood, M. (2022) Intermediate Statistics with R, Version 3.1. https://doi.org/10.15788/2019.08.19
#' @source http://www.wcc.nrcs.usda.gov/snotel/Montana/montana.html
#' @keywords data snow temperature elevation
#' @examples
#' library(GGally)
#' # Reorder columns slightly and only plot quantitative variables using "columns = ..."
#' snotel_s %>% ggpairs(columns = c(4:6,3)) +
#' theme_bw()
#' library(dplyr)
#' library(viridis)
#' snotel_s <- snotel_s %>% mutate(highelevation = factor(ifelse(Elevation > 6000, "High", "Low")))
#' plot2 <- snotel_s %>% ggpairs(columns = c(4:6, 3),
#'                               mapping = aes(color = highelevation, alpha = 0.4),
#'                                                           upper = list(continuous = GGally::wrap(ggally_cor,                                                  size = 4, stars = F))) +
#'                                                           theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#'                                                           theme_bw(base_size = 6) +
#'                                                           scale_color_viridis_d(end = 0.8, option = "H")
#'  plot2
"snotel_s"
