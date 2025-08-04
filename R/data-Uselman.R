#' Data on grass density from split-split-split plot design study from Uselman et al (2018)
#'
#' These data come from a designed experiment conducted by Uselman et al. (2018) to examine the effects of seeding strategy and irrigation on vegetation of various kinds (grass and shrub density in particular). These data examine experimental factors such as seeding strategy, irrigation type, and the origin of grasses and shrubs. It also includes grass, shrub, and weed density measurements collected from 2014 to 2017. The study aims to assess how different land management practices influence/effect vegetation establishment and growth.
#'
#' @format a \code{data.frame} with 504 observations on 19 variables:
#' \describe{
#'   \item{Date}{Sampling date}
#'   \item{Site}{Location of the study (e.g., North, South)}
#'   \item{Block}{Experimental block identifier}
#'   \item{SubBlock}{Sub-block identifier including seeding strategy and timing}
#'   \item{Plot}{Plot number within the sub-block}
#'   \item{Subplot}{Subplot identifier within the plot}
#'   \item{SeedingStrat}{Seeding strategy applied (e.g., I, II)}
#'   \item{Irrigation}{Irrigation timing (e.g., Fall-Spring)}
#'   \item{GrassOrig}{Origin of the grass seeds (e.g., Wild-collected, Commercial)}
#'   \item{ShrubOrig}{Origin of the shrub seeds (e.g., Local, Distant)}
#'   \item{GrassDens2014}{Grass density measured in 2014}
#'   \item{GrassDens2016}{Grass density measured in 2016}
#'   \item{GrassDensNoSPAI2017}{Grass density in 2017, excluding SPAI species}
#'   \item{GrassDens2017}{Total grass density measured in 2017}
#'   \item{ShrubDens2016}{Shrub density measured in 2016}
#'   \item{AllShrubSoField2017}{Shrub density including field shrubs in 2017}
#'   \item{WeedDens2016}{Weed density measured in 2016}
#'   \item{WeedDens2017}{Weed density measured in 2017}
#'   \item{WeedHeight2016}{Weed height measured in 2016}
#' }
#' @name Uselman
#' @docType data
#' @references
#' Uselman SM, Davison J, Baughman OW, Sullivan BW, Miller WW, et al. (2018) Restoring dryland old fields with native shrubs and grasses: Does facilitation and seed source matter?. PLOS ONE 13(10): e0205760. https://doi.org/10.1371/journal.pone.0205760
#' @source https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0205760, data for R package prepared by Amelia Pomazal
#' @keywords grass shrub density split plot data mixed models interaction
#' @examples
#'
#' library(tidyverse)
#' library(forcats)
#' library(lmerTest)
#' library(effects)
#' library(nlme)
#' library(DiagrammeR)
#' library(DiagrammeRsvg)
#' library(webshot)
#' #remotes::install_github("glinse-stat/modeldiagramR")
#' library(modeldiagramR)
#' library(catstats2)
#'
#' data(Uselman)
#'
#'
#' UselmanR <- Uselman %>% dplyr::filter(Site == "South") %>% #South field only (North requires similar code)
#' mutate(Date = factor(Date)) %>%
#' dplyr::filter(Date == "16-May") %>% #Only 2016 observations
#' mutate(SeedingStrat = factor(SeedingStrat),
#'          Irrigation = factor(Irrigation),
#'          GrassOrig = factor(GrassOrig),
#'          Blockf = factor(Block),
#'          logShrubDens25 = log(ShrubDens2016 + 0.025),
#'          Block = factor(Block),
#'          SubBlock = factor(SubBlock),
#'          Plot = factor(Plot))
#'
#' UselmanR <- UselmanR %>%  mutate(SeedingStrat = factor(SeedingStrat),
#'                                  Irrigation = factor(Irrigation),
#'                                  ShrubOrig = factor(ShrubOrig),
#'                                  SeedingStrat.ShrubOrig.GrassOrig = factor(str_c(SeedingStrat, ShrubOrig, GrassOrig)))
#' UselmanR2 <- UselmanR %>% mutate(SeedingStrat.ShrubOrig.GrassOrig =
#'               forcats::fct_na_value_to_level(SeedingStrat.ShrubOrig.GrassOrig, level = "V")) #Turning NAs in shrub origin to 5th level of none
#'
#' # Interaction Plot
#' ggintplot(response = "logShrubDens25", groupvars = c("SeedingStrat.ShrubOrig.GrassOrig", "Irrigation"), data = UselmanR2)
#'
#'
#' m1 <- lmer(logShrubDens25 ~ Irrigation*SeedingStrat.ShrubOrig.GrassOrig +  (1|Block/SubBlock/Plot), data = UselmanR2)
#' model_diagram(m1)
#' anova(m1)
#' plot(allEffects(m1, residuals = T), grid = T, x.var = "SeedingStrat.ShrubOrig.GrassOrig")
#'
"Uselman"
