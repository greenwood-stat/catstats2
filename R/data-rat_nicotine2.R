#' Data from rat tibia data sets from Barra et al. (2025) - Dataset 2 of 3
#'
#' These data contain measurements related to bone health and gastrointestinal conditions in rats subjected to nicotine exposure and coenzyme Q10 supplementation. See original paper for use of these data in figures 9, 10, and 11.
#'
#' @format a \code{data.frame} with 56 observations on 7 variables:
#' \describe{
#'   \item{SUBSTANCE}{treatment substance ('SS': saline solution; 'NIC': nicotine)}
#'   \item{SOLUTION}{'VG': vegetable glycerin; 'Q10': coenzyme Q10}
#'   \item{EUTHANASIA}{timing of euthanasia (days post-treatment)}
#'   \item{SAMPLE}{sample number}
#'   \item{MAX_STRENGTH}{maximum tibial strength (N)}
#'   \item{MAX_RESISTANCE}{maximum tibial resistance (N)}
#'   \item{MODULUS_ELASTICITY}{modulus of elasticity (MPa)}
#' }
#'
#' @name rat_nicotine2
#' @docType data
#' @references
#' Barra, R. H. D., Piovezan, B. R., Matheus, H. R., Vitória, O. A. P., de Abreu Furquim, E. M., Fiorin, L. G., Santos, E. O., & de Almeida, J. M. (2025). Effect of coenzyme Q10 on tibial fracture resistance in nicotine-exposed rats. _PLoS ONE, 20_(1), e0315462. https://doi.org/10.1371/journal.pone.0315462
#' @source \url{https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0315462}, data for R package prepared by Amelia Pomazal
#' @keywords data coenzyme Q10 nicotine bone health rats
#'
#' @examples
#'
#' library(tidyverse)
#' data(rat_nicotine2)
#' rat_nicotine2 <- rat_nicotine2 %>%
#'   mutate(GROUP = paste(SUBSTANCE, SOLUTION, EUTHANASIA, sep = "_"),
#'      GROUP = factor(GROUP))
#'
#' # View two-way interactions between the treatment levels
#'  ggintplot(response = "MAX_RESISTANCE", groupvars = c("SUBSTANCE", "SOLUTION", "EUTHANASIA"), data = rat_nicotine2)
#'
#' # Enhanced stripchart of tibial strength across groups
#'   enhanced_stripchart(data = rat_nicotine2, MAX_STRENGTH ~ GROUP) +
#'      labs(title = "Tibial Weight Across Treatment Groups",
#'        x = "Experimental Group",
#'        y = "Maximum Tibial Strength (N)")
#'
#'
"rat_nicotine2"
