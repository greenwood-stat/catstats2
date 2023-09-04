#' Data on rat head size growth over time
#'
#' Rat data set used in:
#'  Verbeke and Lesaffre (1999), Applied Statistics, 48, 363-375
#'  Verbeke and Molenberghs (2000), New-York: Springer-Verlag
#'  Verbeke and Molenberghs (2003), Biometrics, 59, 254-262
#'  Gelman et al (2005), Biometrics, 61, 74-85
#'  Fahrmeir, L., Kneib, T., Lang, S., & Marx. B. (2022) Regression: Models, Methods and Applications, Second Edition, corrected. Springer.
#'
#'  Variables:
#'  Note: observation number removed from posted data
#'  (2) treat: treament group ('con': control; 'hig': high dose; 'low': low dose)
#'  (3) rat: rat identification number
#'  (4) age: age of the rat at the moment the observation is made
#'  (5) respons: the response measured (distance between two points in "pixels")
#'
#' See papers for information on variables in the data set.
#'
#' @format a \code{data.frame} with 252 observations on 4 variables:
#' \describe{
#'   \item{treat}{treament group ('con': control; 'hig': high dose; 'low': low dose)}
#'   \item{rat}{rat identification number}
#'   \item{age}{age of the rat at the moment the observation is made}
#'   \item{response}{the response measured (distance between two points in "pixels")}
#'
#' }
#'
#' @name rat_decapeptyl
#' @docType data
#' @references Verbeke and Lesaffre (1999), Applied Statistics, 48, 363-375
#' @source Data downloaded from https://gbiomed.kuleuven.be/english/research/50000687/50000696/geertverbeke/datasets
#' @keywords data repeated measures mixed model
#' @examples
#' library(tidyverse)
#' library(catstats2)
#' data(rat_decapeptyl)
#'
#' rat_dec <- rat_decapeptyl
#' rat_dec <- rat_dec %>% mutate(treat = factor(treat),
#'                               ind_control = ifelse(treat == "con", 1, 0),
#'                               ind_high = ifelse(treat == "hig", 1, 0),
#'                               ind_low = ifelse(treat == "low", 1, 0),
#'                               age_t = log(1 + (age - 45)/10),
#'                               age_t_c = age_t*ind_control,
#'                               age_t_h = age_t*ind_high,
#'                               age_t_l = age_t*ind_low)
#'
"rat_decapeptyl"
