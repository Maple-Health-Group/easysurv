% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fit_splines.R
\name{fit_splines}
\alias{fit_splines}
\title{Fit spline models in survival analysis for health economic evaluations}
\usage{
fit_splines(formula, dists, data, method = "mle", weights = NULL)
}
\arguments{
\item{formula}{A formula specifying the model to be used, in the form
\code{Surv(time, event) ~ strata[+covariates]} in flexsurv terms.}

\item{dists}{A data frame with columns \code{knots} and \code{scale}
specifying the number of knots to be used and the type of spline model to be
executed respectively. The possible models are: Proportional hazards model
(\code{scale = "hazard"}), probit model (\code{scale = "normal"}),
proportional odds model (\code{scale = "odds"}).}

\item{data}{A data frame containing the data to be used for the analysis.
This must contain data for the \code{time} and \code{event} variables.}

\item{method}{Method from survHE, default is mle.}

\item{weights}{Optional for case weights. The function expects a string
corresponding to a variable name within the data.}
}
\value{
A list containing the fitted spline models and model fitting
information.
}
\description{
This function fits spline models specified by different knot configurations
to survival data.
}
\examples{
\dontrun{
library(survival)

formula <- Surv(time, status) ~ as.factor(sex)
dists <- data.frame(knots = c(3, 5), scale = rep("normal", 2))
spline_fit <- fit_splines(formula, dists, data = lung)
print(spline_fit)
}

}
