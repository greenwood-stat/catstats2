#' Vehicle passing distances of a bicycle based on various aspects of outfit
#'
#' These data were collected from a study (Walker, Garrard, and Jowitt 2014) to investigate whether clothing worn by a bicyclist might impact the passing distance of cars. One of the authors wore seven different outfits (outfit for the day was chosen randomly by shuffling seven playing cards) on his regular 26 km commute near London in the United Kingdom. Using a specially instrumented bicycle, they measured how close the vehicles passed to the widest point on the handlebars. The seven outfits (“conditions”) that you can view at https://www.sciencedirect.com/science/article/pii/S0001457513004636 were:
#' COMMUTE: Plain cycling jersey and pants, reflective cycle clips, commuting helmet, and bike gloves.
#' CASUAL: Rugby shirt with pants tucked into socks, wool hat or baseball cap, plain gloves, and small backpack.
#' HIVIZ: Bright yellow reflective cycle commuting jacket, plain pants, reflective cycle clips, commuting helmet, and bike gloves.
#' RACER: Colorful, skin-tight, Tour de France cycle jersey with sponsor logos, Lycra bike shorts or tights, race helmet, and bike gloves.
#' NOVICE: Yellow reflective vest with “Novice Cyclist, Pass Slowly” and plain pants, reflective cycle clips, commuting helmet, and bike gloves.
#' POLICE: Yellow reflective vest with “POLICEwitness.com – Move Over – Camera Cyclist” and plain pants, reflective cycle clips, commuting helmet, and bike gloves.
#' POLITE: Yellow reflective vest with blue and white checked banding and the words “POLITE notice, Pass Slowly” looking similar to a police jacket and plain pants, reflective cycle clips, commuting helmet, and bike gloves.
#' They collected data (distance to the vehicle in cm for each car “overtake”).
#'
#' @format a \code{data.frame} with 5690 observations on 8 variables:
#' \describe{
#'   \item{Condition}{Name of outfit}
#'   \item{Distance}{Distance of the vehicle to the bicycle in cm}
#'   \item{Shirt}{Shirt type}
#'   \item{Helmet}{Helmet "type"}
#'   \item{Pants}{Pants type}
#'   \item{Gloves}{Gloves type}
#'   \item{ReflectClips}{Reflective pants clips or not}
#'   \item{Backpack}{Backpack or not}
#' }
#'
#' @name cardist_2014
#' @docType data
#' @references Walker, Ian, Ian Garrard, and Felicity Jowitt. 2014. “The Influence of a
#'  Bicycle Commuter’s Appearance on Drivers’ Overtaking Proximities:
#'  An on-Road Test of Bicyclist Stereotypes, High-Visibility Clothing and Safety Aids in the United Kingdom.”
#'  Accident Analysis & Prevention 64: 69–77. https://doi.org/https://doi.org/10.1016/j.aap.2013.11.007.
#'
#' @keywords data safety vehicle bicycle
#' @examples
#' library(yarrr)
#' pirateplot(Distance ~ Condition, data = cardist_2014, inf.method = "ci", inf.disp = "line")
"cardist_2014"
