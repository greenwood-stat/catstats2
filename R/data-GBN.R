#' Data from journal citations data set from Gundale, Bach, and Nordin (2013)
#'
#' These data are on feathermoss from an experiment
#'
#' @format a \code{data.frame} with 36 observations on 5 variables:
#' \describe{
#'   \item{Species}{Species of Feathermoss}
#'   \item{Treatment}{Amount of nitrogen added including control}
#'   \item{Block}{Block in field}
#'   \item{Massperha}{Mass per hectare}
#'   \item{Nfixpermass}{Nitrogen fixation per mass}
#' }
#'
#' @name GBN
#' @docType data
#' @references
#' Gundale, M., Bach, L., and Nordin, A. (2013) The impact of simulated chronic nitrogen deposition on the biomass and N_2-fixation activity of two boreal feather moss-cyanobacteria associations. Biology Letters 9:20130797 (http://dx.doi.org/10.1098/rsbl.2013.0797).
#' @source https://royalsocietypublishing.org/action/downloadSupplement?doi=10.1098%2Frsbl.2013.0797&file=rsbl-2013-0797-file006.xlsx
#' @keywords data nitrogen interaction
#'
#' @examples
#'  # Data wrangling
#'  GBN <- GBN %>% mutate(Species = factor(Species),
#'       Treatment = factor(Treatment),
#'       Block = factor(Block),
#'       logNfixpermass = log(Nfixpermass)
#'       )
#'  GBN %>% ggintplot(response = "logNfixpermass", groupvars = c("Species", "Treatment"))
#'  #Not run
#'  #GBN %>% ggintplot(response = "logNfixpermass", groupvars = c("Species", "Treatment", "Block"), array = T)
"GBN"
