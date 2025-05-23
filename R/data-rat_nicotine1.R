#' Data from rat tibia data sets from Barra et al. (2025) - Dataset 1 of 3
#'
#' These data contain measurements related to bone health and gastrointestinal conditions in rats subjected to nicotine exposure and coenzyme Q10 supplementation. See original paper for use of these data in figures 5, 6, 7, and 8.
#'
#' @format a \code{data.frame} with 56 observations on 8 variables:
#' \describe{
#'   \item{SUBSTANCE}{treatment substance ('SS': saline solution; 'NIC': nicotine)}
#'   \item{SOLUTION}{'VG': vegetable glycerin; 'Q10': coenzyme Q10}
#'   \item{EUTHANASIA}{timing of euthanasia (days post-treatment)}
#'   \item{SAMPLE}{sample number}
#'   \item{WIDTH}{width of the tibia (mm)}
#'   \item{THICKNESS}{thickness of the tibia (mm)}
#'   \item{LENGTH}{length of the tibia (mm)}
#'   \item{WEIGHT}{weight of the tibia (g)}
#' }
#'
#' @name rat_nicotine1
#' @docType data
#' @references
#' Barra, R. H. D., Piovezan, B. R., Matheus, H. R., Vitória, O. A. P., de Abreu Furquim, E. M., Fiorin, L. G., Santos, E. O., & de Almeida, J. M. (2025). Effect of coenzyme Q10 on tibial fracture resistance in nicotine-exposed rats. _PLoS ONE, 20_(1), e0315462. https://doi.org/10.1371/journal.pone.0315462
#' @source \url{https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0315462}, data for R package prepared by Amelia Pomazal
#' @keywords data coenzyme Q10 nicotine bone health rats
#'
#' @examples
#'
#' # Create a treatment group variable
#' library(tidyverse)
#' data(rat_nicotine1)
#' rat_nicotine1 <- rat_nicotine1 %>%
#'   mutate(GROUP = paste(SUBSTANCE, SOLUTION, EUTHANASIA, sep = "_"),
#'     GROUP = factor(GROUP))
#'
#' # View two-way interactions between the treatment levels
#' ggintplot(response = "WEIGHT", groupvars = c("SUBSTANCE", "SOLUTION", "EUTHANASIA"), data = rat_nicotine1)
#'
#' # Enhanced stripchart of weight across groups
#' enhanced_stripchart(data = rat_nicotine1, WEIGHT ~ GROUP) +
#'     labs(title = "Tibial Weight Across Treatment Groups",
#'      x = "Experimental Group",
#'      y = "Tibial Weight (g)")
#'
"rat_nicotine1"
