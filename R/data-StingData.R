#' Data on bee sting pain ratings by body location
#'
#' These data come from (Smith 2014) where the author experimented on himself
#'  by daily stinging himself five times on randomly selected body locations
#'   over the course of months. You can read more about this fascinating
#'    (and cringe inducing) study at https://peerj.com/articles/338/. The
#'    following code gets the data prepared for analysis by removing
#'    the observations he took each day on how painful it was to sting
#'     himself on his forearm before and after the other three observations
#'      that were of interest each day of the study.
#'      This is done with a negation (using “!” of the %in% which
#'      identifies rows related to the two daily forearm locations
#'      (Forearm and Forearm1) to leave all the rows in the data set
#'      for any levels of Body_Location that were not in these two levels.
#'      This is easier than trying to list all 24 other levels, then
#'      the Body_Location variable is re-factored to clean out its unused levels,
#'      and finally the reorder function is used to order the levels based on
#'      the sample mean pain rating. Note that a couple of observations were corrected
#'      from the originally posted version of the data set on the website.
#'
#' Body_Location = 24 levels of locations with measurements once calibration locations are removed
#' Side = L or R (or NA if side of body is not applicable)
#' Date = Date of measurement
#' Time = Time of day
#' Round = Round of set of measurements
#' Rating = Pain rating on an ordinal scale
#' OrderinDay = Order of measurements taken on a given day
#'
#'
#' @format a \code{data.frame} with 72 observations on 7 variables:
#' \describe{
#'   \item{Body_Location}{Description of location, 24 levels}
#'   \item{Side}{Side of body, where applicable, L (Left) or R (Right) or NA}
#'   \item{Date}{Date}
#'   \item{Time}{Time}
#'   \item{Round}{Round of measurements, three levels}
#'   \item{Rating}{Paint Rating, ordinal scale, higher ratings for more pain}
#'   \item{OrderinDay}{Order of observation within the day}
#' }
#'
#' @name StingData
#' @docType data
#' @references Smith ML. 2014. Honey bee sting pain index by body location. PeerJ 2:e338 https://doi.org/10.7717/peerj.338
#' @source https://peerj.com/articles/338/#supplemental-information
#' @keywords data ANOVA repeated measures pain rating ordinal
#' @examples
#' library(tidyverse)
#' library(catstats2)
#' StingDataR <- StingData %>%
#'   filter(!(Body_Location %in% c("Forearm", "Forearm1"))) %>%
#'       mutate(Body_Location = factor(Body_Location),
#'       Body_Location = reorder(Body_Location, Rating, FUN = mean)
#'       )
#'
#' enhanced_stripchart(Rating ~ Body_Location, data = StingDataR)
#' enhanced_stripchart(Rating ~ Side, data = StingDataR)
"StingData"
