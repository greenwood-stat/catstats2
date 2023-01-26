#' Passing distance of cars relative to bicycle from Walker 2007
#'
#' These data come from Walker (2007) and were discussed in a suite of papers cited below.
#'
#' @format a \code{data.frame} with 2355 observations on 11 variables:
#' \describe{
#'   \item{vehicle}{Type of vehicle, details of levels in code below}
#'   \item{colour}{Color of vehicle, details of levels in code below}
#'   \item{passing distance}{Distance between vehicle and bicycle}
#'   \item{street}{Type of street, details of levels in code below}
#'   \item{Time}{Time of day (year is an artifact)}
#'   \item{hour}{Hour of day}
#'   \item{helmet}{Helmet usage, details of levels in code below}
#'   \item{kerb}{Distance to kerb (curb) of bicycle}
#'   \item{Bikelane}{Bikelane presence, details of levels in code below}
#'   \item{City}{City of measurement, details of levels in code below}
#'   \item{date}{Date of measurement}
#' }
#'
#' @name w2007
#' @docType data
#' @references
#' Walker, I. (2007) Drivers overtaking bicyclists: Objective data on the effects of riding position, helmet use, vehicle type and apparent gender, Accident Analysis & Prevention, 39 (2), 417-425, https://doi.org/10.1016/j.aap.2006.08.010. (Original source of data)
#'
#' Olivier J, Walter SR (2013) Bicycle Helmet Wearing Is Not Associated with Close Motor Vehicle Passing: A Re-Analysis of Walker, 2007. PLoS ONE 8(9): e75424. https://doi.org/10.1371/journal.pone.0075424. (Re-analysis of earlier data)
#'
#' Code used in 2013 paper provided at https://www.researchgate.net/publication/257835919_R_Syntax_for_Walker_Reanalysis
#'
#' Olivier J, Walter SR (2014) Correction: Bicycle Helmet Wearing Is Not Associated with Close Motor Vehicle Passing: A Re-Analysis of Walker, 2007. PLOS ONE 9(1): https://doi.org/10.1371/annotation/7e009550-a92d-49a2-8053-e6fcf7612966 (Correction to 2013 paper)
#'
#' Walker, I and Robinson, D. (2019) Bicycle helmet wearing is associated with closer overtaking by drivers: A response to Olivier and Walter, 2013, Accident Analysis & Prevention, 123, 107-113, https://doi.org/10.1016/j.aap.2018.11.015. (Response to the re-analysis in 2013/2014 papers)
#' @source http://drianwalker.com/overtaking/PsychBikeData.xls
#' @keywords data vehicle bicycle distance helmet
#'
#' @examples
#'  # Data wrangling
#' data(w2007)
#' library(tidyverse)
#' library(mosaic)
#' library(readxl)
#' library(patchwork)
#' library(lubridate)
#' w2007 <- w2007 %>% mutate(Veh = factor(case_when(
#' vehicle %in% c(1, 3, 6, 7) ~ "Small",
#' vehicle %in% c(2, 4, 5) ~ "Large"
#' )),
#' vehicle = factor(vehicle),
#' vehicle = fct_recode(vehicle,
#'                      "ordinary_car" = "1",
#'                      "LGV_minibus" = "2",
#'                      "SUV_pickup" = "3",
#'                      "bus" = "4",
#'                      "HGV" = "5",
#'                      "taxi" = "6",
#'                      "PTW" = "7"),
#' closepass1 = factor(case_when(
#'   "passing distance" < 1 ~ "Close1",
#'   "passing distance" >= 1 ~ "NotClose1")),
#' closepass1.5 = factor(case_when(
#'   "passing distance" < 1.5 ~ "Close1.5",
#'   "passing distance" >= 1.5 ~ "NotClose1.5")),
#' colour = factor(colour),
#' colour = fct_recode(colour,
#'                     "blue" = "1",
#'                     "red" = "2",
#'                     "sliver_grey" = "3",
#'                     "white" = "4",
#'                     "black" = "5",
#'                     "green" = "6",
#'                     "other" = "9",
#'                     "unknown" = "99"),
#' street = factor(street),
#' street = fct_recode(street,
#'                     "one_way_one_lane" = "1",
#'                     "one_way_two_lanes" = "2",
#'                     "regular_urban_street" = "3",
#'                     "regular_residential_street" = "4",
#'                     "main_road_regular" = "5",
#'                     "rural" = "6"),
#' helmet = factor(helmet),
#' helmet = fct_recode(helmet,
#'                     "helmet_on" = "1",
#'                     "no_helmet" = "0"),
#' kerbf = factor(kerb),
#' Bikelane = factor(Bikelane),
#' Bikelane = fct_recode(Bikelane,
#'                       "no_bikelane" = "0",
#'                       "bikelane_present" = "1"),
#' City = factor(City),
#' City = fct_recode(City,
#'                   "Salisbury" = "1",
#'                   "Bristol" = "2"),
#' Hour = hour(hour),
#' TimeofDay = factor(case_when(
#'   Hour >= 14 ~ "Late",
#'   Hour %in% c(7:9) ~ "Morning",
#'   Hour %in% c(10:14) ~ "Middle")),
#' datef = factor(date)) %>%
#'   rename(passdist = "passing distance")
#'
#' table(w2007$helmet, w2007$kerb)
#'
#' t1 <- table(w2007$date, w2007$City)
#'
#' weatherdata <- data.frame(DateofMeasurement = factor(rownames(t1)),
#'                           CityofMeasurement = factor(ifelse(t1[,2] > 0, "Salisbury", "Bristol")),
#'                           HighTempF = c(70, 70, 61, 54, 59, 55, 55, 57, 68, 68, 77, 61),
#'                           LowTempF = c(48, 50, 50, 45, 48, 52, 52, 41, 50, 46, 46, 54),
#'                           MaxWindSpeedmph = c(13, 15, 23, 31, 12, 26, 21, 15, 12, 13, 8, 14)
#' ) #Info from weatherunderground for each day
#'
#' w2007 <- left_join(x = w2007, y = weatherdata, by = c("datef" = "DateofMeasurement"))
#'
#' #favstats(Hour ~ TimeofDay, data = w2007) #Note: this may not match papers - no code found
#'
#' p1 <- w2007 %>% ggplot(aes(x = kerbf, y = passdist, color = helmet)) +
#'   geom_violin(position = position_dodge(0.5)) +
#'   geom_point(position = position_dodge(0.5), alpha = .1) +
#'   scale_color_viridis_d()
#'
#' p2 <- w2007 %>% ggplot(aes(x = MaxWindSpeedmph, y = passdist)) +
#'   geom_point() +
#'   geom_smooth(method = "lm")
#'
#' p1 + p2
#'
#' p3 <- w2007 %>% ggplot(aes(x = helmet, y = passdist)) +
#'   geom_violin() +
#'   geom_jitter() +
#'   facet_wrap(~datef, nrow = 3)
#'
#' p3
"w2007"
