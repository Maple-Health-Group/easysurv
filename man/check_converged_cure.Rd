% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_converged_cure.R
\name{check_converged_cure}
\alias{check_converged_cure}
\title{Check whether cure models converge for survival analysis}
\usage{
check_converged_cure(formula, data, dists)
}
\arguments{
\item{formula}{A formula specifying the model to be used, in the
form \code{Surv(time, event) ~ strata[+covariates]} in flexsurv terms.}

\item{data}{A data frame containing the data to be used for the analysis.
This must contain data for the \code{time} and \code{event} variables.}

\item{dists}{Name or vector of names for the distribution(s) to be used.}
}
\value{
A vector of distribution names for which the corresponding cure
models have converged.
}
\description{
This function checks whether cure models specified by different
distributions converge.
}
\examples{
\dontrun{
library(survival)

check_converged_cure(
  formula = Surv(time, status) ~ as.factor(sex),
  data = lung[1:7,],
  dists = c("exponential", "weibull", "gengamma", "lognormal")
)
}

}
