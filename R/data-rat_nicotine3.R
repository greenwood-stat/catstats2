#' Data from rat tibia data sets from Barra et al. (2025) - Dataset 3 of 3
#'
#' These data contain measurements related to bone health and gastrointestinal conditions in rats subjected to nicotine exposure and coenzyme Q10 supplementation.  See original paper for use of these data in figures 12, 13, and 14.
#'
#' @format a \code{data.frame} with 56 observations on 7 variables:
#' \describe{
#'   \item{SUBSTANCE}{treatment substance ('SS': saline solution; 'NIC': nicotine)}
#'   \item{SOLUTION}{'VG': vegetable glycerin; 'Q10': coenzyme Q10}
#'   \item{EUTHANASIA}{timing of euthanasia (days post-treatment)}
#'   \item{SAMPLE}{sample number}
#'   \item{TOTAL_AREA_PERCENT}{total bone area (%)}
#'   \item{PERIMETER}{tibial perimeter (mm)}
#'   \item{POROSITY}{combined measure of porosity and bone mineral density (%)}
#' }
#'
#' @name rat_nicotine3
#' @docType data
#' @references
#' Barra, R. H. D., Piovezan, B. R., Matheus, H. R., Vitória, O. A. P., de Abreu Furquim, E. M., Fiorin, L. G., Santos, E. O., & de Almeida, J. M. (2025). Effect of coenzyme Q10 on tibial fracture resistance in nicotine-exposed rats. _PLoS ONE, 20_(1), e0315462. https://doi.org/10.1371/journal.pone.0315462
#' @source \url{https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0315462}, data for R package prepared by Amelia Pomazal
#' @keywords data coenzyme Q10 nicotine bone health rats
#'
#' @examples
#'
#' library(tidyverse)
#' data(rat_nicotine3)
#' rat_nicotine3 <- rat_nicotine3 %>%
#'   mutate(GROUP = paste(SUBSTANCE, SOLUTION, EUTHANASIA, sep = "_"),
#'      GROUP = factor(GROUP))
#'
#' # Summary of bone area percentage by group
#' rat_nicotine3 %>%
#'   group_by(GROUP) %>%
#'   summarize(mean_area = mean(TOTAL_AREA_PERCENT, na.rm = TRUE),
#'             mean_porosity = mean(POROSITY, na.rm = TRUE))
#'
#' # Enhanced stripchart of porosity across groups
#'   enhanced_stripchart(data = rat_nicotine3, POROSITY ~ GROUP) +
#'     labs(title = "Bone Porosity Across Treatment Groups",
#'        x = "Experimental Group",
#'        y = "Porosity (%)")
#'
"rat_nicotine3"
