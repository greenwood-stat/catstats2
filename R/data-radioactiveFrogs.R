#' Data on Age, Telomere Length, and Corticosterone in Chornobyl Tree Frogs
#'
#' These data come from a field study of Eastern tree frogs (\emph{Hyla orientalis}) sampled from 14 localities in and around the Chornobyl Exclusion Zone between 2016 and 2018.
#' The dataset includes measurements of age (via skeletochronology), individual absorbed radiation dose, and — in a 2018 subset — telomere length and natural log-corticosterone hormone levels.
#' The goal of the study was to assess the long-term impacts of chronic ionizing radiation exposure on physiological and aging-related traits in wild amphibians.
#'
#' @format A \code{data.frame} with 191 observations on 17 variables:
#' \describe{
#'   \item{Population}{Sampling locality name}
#'   \item{Pop_ID}{Numeric code for population/locality}
#'   \item{GPSLat}{Latitude of sampling location, corrected for two locations from published data}
#'   \item{GPSLong}{Longitude of sampling location, corrected for two locations from published data}
#'   \item{Year}{Year of sampling (2016, 2017, or 2018)}
#'   \item{SamplingDate}{Date of frog collection (YYYY-MM-DD)}
#'   \item{Age}{Estimated age in years, determined by counting lines of arrested growth (LAGs) in the femur}
#'   \item{Total Dose Rate}{Estimated total absorbed radiation dose rate (µGy/h), combining internal and external exposure}
#'   \item{Ambient Dose Rate}{Ambient radiation measured at the capture site (µSv/h)}
#'   \item{Radiation_cat}{Radiation category: "Low" (0.01–0.27 µSv/h) or "Medium–High" (1.09–32.4 µSv/h)}
#'   \item{Mass}{Body mass in grams}
#'   \item{SVL}{Snout-vent length in millimeters}
#'   \item{BCI}{Body Condition Index, calculated as residuals from the mass–length regression}
#'   \item{LnCORT}{Natural log-transformed salivary corticosterone concentration (pg/ml), 2018 samples only}
#'   \item{NRQ_TOTAL}{Relative telomere length (Normalized Relative Quantity), measured via qPCR, 2018 samples only}
#'   \item{Dose0}{Levels of "None" (no radiation) or "Some" based on location}
#'   \item{Dose3level}{Radiation dosage based on location, "None" (0 µSv/h), "Some" (>0 but less than or equal to 1.09 µSv/h), or "MediumHigh" (Over 1.09 µSv/h)}
#' }
#' @name radioactiveFrogs
#' @docType data
#' @references
#' Burraco P, Gabor C, Bryant A, Gardette V, Lengagne T, Bonzom JM, Orizaola G. (2024).
#' "Ionizing radiation has negligible effects on the age, telomere length and corticosterone levels of Chornobyl tree frogs."
#' \emph{Biology Letters}, 20: 20240287. \url{https://doi.org/10.1098/rsbl.2024.0287}, dataset prepared for R package by Amelia Pomazal
#' @source \url{https://doi.org/10.1098/rsbl.2024.0287}
#' @keywords radiation ecology amphibians telomeres hormones chornobyl
#' @examples
#'
#' library(tidyverse)
#' library(mi)
#' library(ggResidpanel)
#' library(lmerTest)
#' library(effects)
#'
#' data(radioactiveFrogs)
#'
#'  #Explore missing data pattern:
#'  mid <- missing_data.frame(as.data.frame(radioactiveFrogs))
#'  image(mid)
#'  table(mid@patterns)
#'
#'  enhanced_stripchart(Age ~ Radiation_cat, data = radioactiveFrogs) #Sim to their Fig 1b
#'
#'  # From paper: "To investigate the effect of ambient radiation on age, we conducted a linear mixed model including age as the dependent variable, ambient radiation category as the independent category and locality nested within a year of sampling as the random factor."
#'  lmer1 <- lmer(Age ~ Radiation_cat + (1|Year/Population), data = radioactiveFrogs)
#'  summary(lmer1) #Doesn't quite match results stated - unclear why - maybe different n?
#'
#'  # Other ideas...
#'
#' Massplot <- radioactiveFrogs %>% ggplot(aes(x = Age, y = Mass)) +
#'  geom_point() +
#'  geom_smooth(col = "red", lty = 2, se = F) +
#'  geom_smooth(method = "lm") +
#'   facet_wrap(~Radiation_cat)
#'
#' SVLplot <- radioactiveFrogs %>% ggplot(aes(x = Age, y = SVL)) +
#'  geom_point() +
#'  geom_smooth(col = "red", lty = 2, se = F) +
#'  geom_smooth(method = "lm") +
#'   facet_wrap(~Radiation_cat)
#'
#'   library(patchwork)
#'   Massplot + SVLplot
#'
#' lmerA1 <- lmer(Mass ~ Age*Dose3level + (1|Population/Year), data = radioactiveFrogs)
#' plot(allEffects(lmerA1, residuals = T))
#' summary(lmerA1)
#' anova(lmerA1)
#' resid_panel(lmerA1)
#'
#' lmerB2 <- lmer(SVL ~ Age*Dose3level + (1|Population/Year), data = radioactiveFrogs)
#' plot(allEffects(lmerB2, residuals = T))
#' summary(lmerB2)
#' anova(lmerB2)
#'
'radioactiveFrogs'
