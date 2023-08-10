#' Data on brain size and activity levels of subjects
#'
#' Siddarth et al. (2018) researched the relationship between time spent sitting
#'  (sedentary behavior) and the thickness of a participant’s medial temporal
#'   lobe (MTL) in a 2018 paper entitled, “Sedentary behavior associated with
#'    reduced medial temporal lobe thickness in middle-aged and older adults”.
#'    MTL volume is negatively associated with Alzheimer’s disease and memory
#'     impairment. Key variables include:
#'
#' "TOTAL" = Medial temporal lobe thickness in mm
#' "Sitting" = Reported hours/day spent sitting
#' "METminwk" = Reported metabolic equivalent unit minutes per week
#' "Age" = Age in years
#' "Sex" = Sex (M = Male, F = Female)
#' "Educ" = Years of education completed
#'
#' See paper for information on other variables in the data set.
#'
#' @format a \code{data.frame} with 35 observations on 23 variables:
#' \describe{
#'   \item{Subject}{Unique identifier, one per subject}
#'   \item{Sex}{M/F}
#'   \item{Ethnic}{Caucasian/other}
#'   \item{e4grp}{Apolipoprotein E-4 group (E-4/non-E-4)}
#'   \item{Age}{Age in years}
#'   \item{MMSE}{Mini-mental status exam}
#'   \item{HamD}{Score on Hamilton Depression inventory}
#'   \item{HamA}{Score on Hamilton Anxiety inventory}
#'   \item{DigSymm}{Digit Symbol, scores range from 49 to 72, with higher scores indicating better functioning}
#'   \item{DelayVP}{Verbal Pair Association, scores range from 0 to 8, with higher scores indicating better cognitive functioning}
#'   \item{BFRSelective Reminding Delayed}{Selective reminding, scores range from 0 to 12, with higher scores indicating better cognitive functioning}
#'   \item{Sitting}{Reported sitting hours per day}
#'   \item{METminwk}{Minutes of activity per week}
#'   \item{CSA}{tendon gross morphology (average of three scans), units?}
#'   \item{TOTAL}{Total MTL thickness in mm}
#'
#' }
#'
#' @name sit_and_brain
#' @docType data
#' @references Siddarth P, Burggren AC, Eyre HA, Small GW, Merrill DA (2018) Sedentary behavior associated with reduced medial temporal lobe thickness in middle-aged and older adults. PLoS ONE 13(4): e0195549. https://doi.org/10.1371/journal.pone.0195549
#' @source Data downloaded from https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0195549
#' @keywords data SLR MLR brain size activity level
#' @examples
#' library(tidyverse)
#' library(catstats2)
#' data(sit_and_brain)
#'
#' enhanced_stripchart(TOTAL ~ Sex, data = sit_and_brain)
#'
"sit_and_brain"
