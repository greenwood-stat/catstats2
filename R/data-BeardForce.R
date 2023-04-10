#' Data on impact forces under three hair conditions
#'
#' Data were slightly re-formatted from original data set for easier analysis.
#'  Note that "PossibleID" is for instructional purposes only to explore a made-up subject-to-subject
#'  variability in the samples that was not discussed one way or another in the paper,
#'  although the rows of observations were labeled with this information.
#'
#' PossibleID = Label from 1 to 20 that could correspond to different animals
#' PeakForce = Peak Force in kN
#' PeakEnergy = Peak Energy in J
#' TimetoPeakForce = Time to Peak Force in ms
#' TimetoPeakEnergy = Time to Peak Energy in ms
#'
#'
#' @format a \code{data.frame} with 60 observations on 6 variables:
#' \describe{
#'   \item{PossibleID}{Label from 1 to 20 that could correspond to different animals}
#'   \item{PeakForce}{Peak Force in kN}
#'   \item{PeakEnergy}{Peak Energy in J}
#'   \item{TimetoPeakForce}{Time to Peak Force in ms}
#'   \item{TimetoPeakEnergy}{Time to Peak Energy in ms}
#' }
#'
#' @name BeardForce
#' @docType data
#' @references E A Beseris, S E Naleway, D R Carrier, Impact Protection Potential of Mammalian Hair: Testing the Pugilism Hypothesis for the Evolution of Human Facial Hair, Integrative Organismal Biology, Volume 2, Issue 1, 2020, obaa005, https://doi.org/10.1093/iob/obaa005
#' @source https://datadryad.org/stash/dataset/doi:10.5061/dryad.6djh9w0xn
#' @keywords data ANOVA beard facial hair force
#' @examples
#' library(tidyverse)
#' library(catstats2)
#' BeardForce <- BeardForce %>% mutate(Condition = factor(Condition),
#' logTimetoPeakEnergy = log(TimetoPeakEnergy),
#' logTimetoPeakForce = log(TimetoPeakForce))
#'
#' enhanced_stripchart(PeakForce ~ Condition, data = BeardForce)
#' enhanced_stripchart(TimetoPeakForce ~ Condition, data = BeardForce)
#'
#' anova(mF <- lm(PeakForce ~ Condition, data = BeardForce))
#'
#' anova(mE <- lm(PeakEnergy ~ Condition, data = BeardForce))
#'
#' anova(mTE <- lm(TimetoPeakForce ~ Condition, data = BeardForce))
#' anova(mlTE <- lm(logTimetoPeakForce ~ Condition, data = BeardForce))
#'
#'
#' library(ggResidpanel)
#' resid_panel(mTE, "R")
#' resid_panel(mlTE, "R")
#'
#'
#' BeardForce %>% ggplot(aes(x = logTimetoPeakForce, y = PeakForce,
#'     color = Condition)) +
#'     geom_point() +
#'     geom_smooth()
#'
#' ANCOVA1 <- lm(PeakForce ~ logTimetoPeakForce * Condition, data = BeardForce)
#'
#' anova(ANCOVA1)
#'
#' library(effects)
#' plot(allEffects(ANCOVA1, residuals = T))
#'
#' resid_panel(ANCOVA1, "R")
#'
#'
#' ANCOVA2 <- lm(PeakForce ~ poly(logTimetoPeakForce,2) * Condition, data = BeardForce)
#'
#' plot(allEffects(ANCOVA2, residuals = T))
#'
#' par(mfrow = c(2,2))
#' plot(ANCOVA2)
#'
#' BeardForce %>% mutate(Residuals1 = residuals(ANCOVA1)) %>%
#'   ggplot(aes(x = logTimetoPeakForce, y = Residuals1,
#'               color = Condition)) +
#'               geom_point() +
#'               geom_smooth() +
#'               facet_wrap(~Condition)
#'
"BeardForce"
