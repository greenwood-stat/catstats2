#' Data from study on cold challenges and rates of colds
#'
#' Results of a study of 661 subjects exposed to challenge (infectious/not)
#' and one of three different temperatures (unchilled and two versions of chilled
#' conditions).
#'
#'
#'
#' @format a \code{data.frame} with different observations from 661 subjects:
#' \describe{
#'   \item{Gender}{Gender of subjects, Male or Female}
#'   \item{TempTreatment}{TempTreatment, three levels of Unchilled, Chilled10, and Chilled60}
#'   \item{Challenge}{Challenge, two levels of Infections or Noninfectious}
#'   \item{Outcome}{Outcome of study, either Sick or NotSick}
#' }
#'
#' @name coldcolds
#' @docType data
#' @references  Dowling, H. F., Jackson, G. G., Spiesman, I. G., & Inouye, T. (1958). Transmission of the common cold to volunteers under controlled conditions. III. The effect of chilling of the subjects upon susceptibility. Journal of the American Medical Association, 167(11), 1206–1212.
#' @source Table 1 from paper
#' @keywords data cold infections contingency tables mosaic plots logistic
#' @examples
#' library(ggthemes)
#' library(tidyverse)
#' library(ggplot2)
#' library(mosaic)
#' library(ggmosaic)
#' library(easyalluvial)
#' library(car)
#' data(coldcolds)
#' coldcolds %>% alluvial_wide(fill_by = "last_variable")
#'
#' tally(Outcome ~ str_c(Gender, TempTreatment, Challenge), data = coldcolds)
#'
#' glm1 <- glm(Outcome ~ Gender*TempTreatment*Challenge, data = coldcolds,
#'       family = binomial)
#' summary(glm1)
#' Anova(glm1)
"coldcolds"

