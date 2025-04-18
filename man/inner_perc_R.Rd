% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_diagram_function.R
\name{inner_perc_R}
\alias{inner_perc_R}
\title{Calculates inner group percentage}
\usage{
inner_perc_R(x, grp)
}
\arguments{
\item{x}{Column of X matrix to be assessed}

\item{grp}{Integer vector with groups}
}
\value{
Numeric vector of percentages
}
\description{
\code{inner_perc_R()} Calculates the percentage of groups for which the fixed
effect \code{x} is "inner". Data are assumed to be ordered by \code{grp}.
}
\details{
Translated \code{inner_perc()} from nlme's nlmefit.c file (https://github.com/cran/nlme/blob/master/src/nlmefit.c)
Code translation assistance provided by Anthropic's Claude 3.5 Sonnet (2024 version) on December 17, 2024.
}
\examples{
\dontrun{inner_perc_R(X[, j], grps[, i])}
}
