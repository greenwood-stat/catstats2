% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data-didg_study.R
\docType{data}
\name{didg_study}
\alias{didg_study}
\title{Data from trial of playing didgeridoo/not on sleepiness}
\format{
a \code{data.frame} with 50 observations on 4 variables from 25 subjects:
\describe{
\item{Epworth}{Daytime sleepiness "score" from 0 (no sleepiness) to 24}
\item{Baseline_Followup}{1 is baseline, 2 is followup}
\item{Subject}{Subject ID}
\item{Group}{D for treatment group, C for control}
}
}
\source{
Digitized version of published Figure 3 (does not exactly match published numerical summaries!)
}
\usage{
didg_study
}
\description{
Results of a study of 25 patients aged > 18 years with an
apnoea-hypopnoea index between 15 and 30 and who complained about snoring.
Subjects that met criteria were randomized (stratification used but no information in data) to get didgeridoo lessons and daily practice
at home with standardised instruments for four months or in the control group that remained on the waiting list for lessons.
Main outcome measure Daytime sleepiness (Epworth scale from 0 (no daytime sleepiness) to 24)
}
\details{
epworth score based on the sum of responses to a set Likert scale questions about sleepiness (higher is more sleepy).
}
\examples{
library(ggthemes)
library(tidyverse)
didg_study <- didg_study \%>\% mutate(Baseline_Followup = factor(Baseline_Followup),
    Group = factor(Group), #Note that Group = C for control and Group = D for Didgeridoo
    Subject = factor(Subject))

levels(didg_study$Baseline_Followup) <- c("Baseline", "Followup")
#1 was Baseline and 2 was Followup after four months

Spaghetti <- didg_study \%>\% ggplot(mapping = aes(x = Baseline_Followup, y = Epworth, group = Subject)) +
  geom_line(aes(color = Group)) +
  geom_point(aes(color = Group)) +
  scale_color_colorblind()

Spaghetti_facet <- didg_study \%>\% ggplot(mapping = aes(x = Baseline_Followup, y = Epworth, group = Subject)) +
  geom_line(aes(color = Group)) +
  geom_point(aes(color = Group)) +
  facet_wrap(vars(Group)) +
  scale_color_colorblind()

Spaghetti + Spaghetti_facet

ggintplot(response = "Epworth", groupvars = c("Baseline_Followup", "Group"), data = didg_study)
}
\references{
Puhan M A, Suarez A, Cascio C L, Zahn A, Heitz M, Braendli O et al. Didgeridoo playing as alternative treatment for obstructive sleep apnoea syndrome: randomised controlled trial BMJ 2006; 332 :266 doi:10.1136/bmj.38705.470590.55
}
\keyword{data}
\keyword{didgeridoo}
\keyword{epworth}
\keyword{measures}
\keyword{repeated}
