#' Data from a repeated measures study on postural control
#'
#' Results of a study of repeated measures on 30 subjects with athletic
#'  backgrounds on their postural control in different conditions.
#'
#' Data contains hints of measurement conditions in each column that are processed
#' using the code in the help file.
#'
#'
#' @format a \code{data.frame} with different observations from 30 subjects:
#' \describe{
#'   \item{ID}{Subject ID from 1 to 30}
#'   \item{Sport}{Sports background coded 1 = Baseball, 2 = Soccer, or 3 = Novice}
#'   \item{OQ...2}{Area measurents from Open, Quiet conditions (others defined similarly)}
#' }
#'
#' @name PostControl
#' @docType data
#' @references  Liang Y, Hiley M, Kanosue K (2019) The effect of contact sport expertise on postural control. PLOS ONE 14(2): e0212334. https://doi.org/10.1371/journal.pone.0212334
#' @source https://figshare.com/articles/dataset/The_effect_of_contact_sport_expertise_on_postural_control/7721264?file=14371811
#' @keywords data posture balance repeated measures mixed models
#' @examples
#' library(ggthemes)
#' library(dplyr)
#' library(ggplot2)
#' library(janitor)
#' library(lmerTest)
#' data(PostControl)
#' AreaLong <- PostControl %>% dplyr::select(1:8, 27) %>% pivot_longer(cols = 2:8,
#'                                             names_to = "TestCondition",
#'                                             values_to = "Area")
#' AreaLong <- AreaLong %>% mutate(Sport = factor(Sport),
#'                                 TestCondition = factor(TestCondition),
#'                                 TCc = as.character(TestCondition),
#'                                 Eyes = factor(substr(TCc, start=1, stop=1)), #open/closed
#'                                 Stance = factor(substr(TCc, start=2, stop=5)),
#'                                 logArea = log(Area))
#' AreaLong <- AreaLong %>% mutate(Stance = fct_recode(Stance,
#'                                                    OneLeg = "OL..",
#'                                                    OneLegFoam = "OL_F",
#'                                                    Quiet = "Q...",
#'                                                    Toe = "T..."))
#' AreaLongR <- AreaLong %>% dplyr::filter(Stance != "Toe") %>%
#'             mutate(Stance = factor(Stance),
#'             Stance = fct_reorder(Stance, .fun = mean, Area))
#'
#' COPMLLong <- PostControl %>% dplyr::select(10:17, 27) %>% pivot_longer(cols = 2:8,
#'                                         names_to = "TestCondition",
#'                                         values_to = "COPML")
#' COPMLLong <- COPMLLong %>% mutate(Sport = factor(Sport2),
#'                                   TestCondition = factor(TestCondition),
#'                                   TCc = as.character(TestCondition),
#'                                   Eyes = factor(substr(TCc, start=1, stop=1)), #open/closed
#'                                   Stance = factor(substr(TCc, start=2, stop=5)),
#'                                   logCOPML = log(COPML))
#'
#' COPMLLong <- COPMLLong %>% mutate(Stance = fct_recode(Stance,
#'                                                       OneLeg = "OL..",
#'                                                       OneLegFoam = "OL_F",
#'                                                       Quiet = "Q...",
#'                                                       Toe = "T..."))
#' COPMLLongR <- COPMLLong %>% dplyr::filter(Stance != "Toe") %>%
#'         mutate(Stance = factor(Stance),
#'         Stance = fct_reorder(Stance, .fun = mean, COPML))
#'
#' COPAPLong <- PostControl %>% dplyr::select(19:27) %>% pivot_longer(cols = 2:8,
#'                                                         names_to = "TestCondition",
#'                                                         values_to = "COPAP")
#' COPAPLong <- COPAPLong %>% mutate(Sport = factor(Sport3),
#'                                TestCondition = factor(TestCondition),
#'                                TCc = as.character(TestCondition),
#'                                Eyes = factor(substr(TCc, start=1, stop=1)), #open/closed
#'                                Stance = factor(substr(TCc, start=2, stop=5)),
#'                                logCOPAP = log(COPAP))
#' COPAPLong <- COPAPLong %>% mutate(Stance = fct_recode(Stance,
#'                                OneLeg = "OL..",
#'                                OneLegFoam = "OL_F",
#'                                Quiet = "Q...",
#'                                Toe = "T..."))
#' COPAPLongR <- COPAPLong %>% dplyr::filter(Stance != "Toe") %>%
#'       mutate(Stance = factor(Stance),
#'       Stance = fct_reorder(Stance, .fun = mean, COPAP))
#'
#' ggintplot(response = "Area", groupvars = c("Stance", "Sport", "Eyes"), data = AreaLongR, array = F)
#' ggintplot(response = "COPML", groupvars = c("Stance", "Sport", "Eyes"), data = COPMLLongR, array = F)
#' ggintplot(response = "COPAP", groupvars = c("Stance", "Sport", "Eyes"), data = COPAPLongR, array = F)
#' lmm1 <- lmer(Area ~ Sport*Stance*Eyes + (1|ID), data = AreaLongR)
#' summary(lmm1)
#' anova(lmm1)
"PostControl"

