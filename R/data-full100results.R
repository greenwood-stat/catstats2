#' 100m sprint times in Round 1
#'
#' These data come from a study of lane differences on sprinting by Munro (2022).
#'
#' @format a \code{data.frame} with 1671 observations on 28 variables:
#' \describe{
#'   \item{pos}{unknown}
#'   \item{bib}{Runner bib number}
#'   \item{athlete}{Athlete Name (some character issues present)}
#'   \item{country}{Country of athlete}
#'   \item{mark1}{Time in seconds}
#'   \item{reactiontime}{Reaction time to start in seconds}
#'   \item{wind1}{Wind speed in (probably) meters/second, A race is considered to have the assistance of the wind if a TAILWIND exceeds an average velocity of 2.0 m/s }
#'   \item{meet}{Name of meet}
#'   \item{event}{Name of event in meet, just 100m here - more available from source}
#'   \item{Division}{Division of the competitors, labeled as Men or Women}
#'   \item{round}{Only levels where the lane was probably randomly assigned. See Munro (2022) for more on this.}
#'   \item{lane}{Lane of runner assignment}
#'   \item{pb}{Personal best time in seconds of runner}
#'   \item{sb}{Season best time in seconds of runner}
#'   \item{Lane1}{Indicator with 1 for Lane 1, 0 otherwise, remaining variables similarly defined}
#' }
#'
#' @name full100results
#' @docType data
#' @references
#' Munro D (2022) Are there lane advantages in track and field? PLoS ONE 17(8): e0271670. https://doi.org/10.1371/journal.pone.0271670
#' @source https://github.com/dmunro-git/Lane-Advantages
#' @keywords data lane sprint
#'
#' @examples
#'  # Declare factors, remove some missing values, split by Division
#' data(full100results)
#' library(tidyverse)
#' library(ggplot2)
#' full100results <- full100results %>% mutate(
#'   lane = factor(lane),
#'   country = factor(country),
#'   athlete = factor(athlete),
#'   meet = factor(meet),
#'   event = factor(event),
#'   Division = factor(Division)) %>% drop_na(mark1, reactiontime) #Drop 54 and 1 extra missing on reaction time for missingness - note potential bias for removing these
#' f100results <- full100results %>% filter(Division == "Women")
#' m100results <- full100results %>% filter(Division == "Men")
#' f100results %>% ggplot(aes(x = lane, y = mark1)) +
#'   geom_violin() +
#'   geom_point()
#'   #Using enhanced_stripchart:
#'   p_men <- enhanced_stripchart(mark1 ~ lane, data = m100results) +
#'     labs(title = "Plot of Men's 100m results") +
#'     ylim(9, 15)
#'   p_women <- enhanced_stripchart(mark1 ~ lane, data = f100results) +
#'     labs(title = "Plot of Women's 100m results") +
#'     ylim(9, 15)
#' library(patchwork)
#'  p_women / p_men
"full100results"
