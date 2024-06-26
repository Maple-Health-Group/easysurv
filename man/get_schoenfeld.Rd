% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_model_info.R
\name{get_schoenfeld}
\alias{get_schoenfeld}
\title{Extract Schoenfeld Residuals}
\usage{
get_schoenfeld(fit_zph)
}
\arguments{
\item{fit_zph}{An object of class \code{cox.zph} produced by the \code{cox.zph}
function, representing the Schoenfeld residuals of a Cox proportional
hazards model.}
}
\value{
A tibble with the Schoenfeld residuals in long format, containing
the columns:
\item{time}{The time variable from the Cox model.}
\item{transform}{The transformation applied to the time variable.}
\item{variable}{The variable names from the Cox model for which residuals
are calculated.}
\item{residual}{The Schoenfeld residuals for each variable at each time
point.}
}
\description{
This function extracts Schoenfeld residuals from a fitted \code{cox.zph} object
and formats them into a tidy data frame.
}
\examples{
library(survival)
test_fit <- survival::coxph(survival::Surv(time, status) ~ sex, data = lung)
test_fit_zph <- survival::cox.zph(test_fit)
get_schoenfeld(test_fit_zph)
}
