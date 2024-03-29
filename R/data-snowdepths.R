#' Snow depth and density measurements at various sites along with site characteristics
#'
#' These data were collected using a stratified sampling plan as discussed in Wetlaufer et al. (2016).
#' More details on the sampling design and analysis used are in the original paper.
#' Data graciously provided by authors.
#'
#' Likely match to the data published in Appendix A of https://scholarworks.montana.edu/xmlui/handle/1/3470
#'
#' @format a \code{data.frame} with 1017 observations on 10 variables:
#' \describe{
#'   \item{swe}{Snow water equivalent in [?]}
#'   \item{elev}{Elevation of sampling location in study area in meters above sea level}
#'   \item{cover}{Trees or not (10 is tree covered, 0 is not) based on GIS data}
#'   \item{rad}{Radiation from GIS data for each location, see paper for definition}
#'   \item{curv}{Curvature of location based on GIS data, see paper and ArcGIS description for definition of curvature}
#'   \item{aspect}{Angular (degrees) for orientation of slope with 0/360 for ? and 90 for ? - see calculation of Aspect as categorical}
#'   \item{angle}{Angular (degrees) for steepness of slope}
#'   \item{strata}{Codes for strata identification of location, see paper}
#'   \item{snow}{Snow depth in ?}
#'   \item{density}{Density of snow in ?, missing when snow depth is 0}
#'
#' }
#'
#' @name snowdepths
#' @docType data
#' @references Wetlaufer, K., Hendrikx, J., and L. Marshall (2016)
#' Spatial Heterogeneity of Snow Density and Its Influence on
#' Snow Water Equivalence Estimates in a Large Mountainous Basin.
#' Hydrology, 3(1):3, doi:10.3390/hydrology3010003.
#' Available at http://www.mdpi.com/2306-5338/3/1/3/htm
#' #'
#' @keywords data snow depth aspect
#' @examples
#' library(tidyverse)
#' library(effects)
#' snowdepths <- snowdepths %>%
#'   mutate(AspectCat = factor(case_when(
#'     aspect %in% (0:45)~ "North",
#'     aspect %in% (315:360)~ "North",
#'     aspect %in% 45:(90+45) ~ "East",
#'     aspect %in% (90+45):(180+45) ~ "South",
#'     aspect %in% (180+45):315 ~ "West"
#'   )),
#'   SnowPresence = factor(case_when(
#'     snow == 0 ~ "None",
#'     snow > 0 ~ "Some"
#'   )),
#'   Landf = factor(cover)
#'   )
#'levels(snowdepths$Landf) <- c("Not Forested", "Forested")
#'
#'plot(SnowPresence ~ Landf, data = snowdepths, main = "Measurable Snow versus Forested Area", ylab = "Snow Present", xlab = "Forested Area")
#'snowdepths <- snowdepths %>% mutate(ElMean = ave(elev, strata),
#'       ElevCent = elev - ElMean)
#'
#'glm2 <- glm(SnowPresence ~ Landf + rad + ElMean + ElevCent + AspectCat,
#'            data = snowdepths, family = binomial)
#'summary(glm2)
#'plot(allEffects(glm2), type = "response", grid = T)
#'
#'snowdepthsR <- snowdepths %>% dplyr::filter(SnowPresence == "Some")
#'densitymodel <- lm(density ~ Landf + rad + ElMean + ElevCent + AspectCat, data = snowdepthsR)
#'plot(allEffects(densitymodel, residuals = T), grid = T)
"snowdepths"
