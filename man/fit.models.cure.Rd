% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fit.models.cure.R
\name{fit.models.cure}
\alias{fit.models.cure}
\title{Fit parametric mixture cure survival models for health economic evaluations}
\usage{
fit.models.cure(
  formula = NULL,
  data,
  distr = NULL,
  method = "mle",
  weights = NULL,
  ...
)
}
\arguments{
\item{formula}{A formula specifying the model to be used, in the form
\code{Surv(time,event)~strata[+covariates]} for /code{flexsurv}. Recommend
that \code{class(strata) == "factor"}.}

\item{data}{A data frame containing the data to be used for the analysis.
This must contain data for the 'event' variable.}

\item{distr}{A (vector of) string(s) containing the name(s) of the model(s)
to be fitted. Available options are: \code{"exponential"},\code{"gamma"},
\code{"genf"}, \code{"gengamma"},\code{"gompertz"},\code{"weibull"},
\code{"weibullPH"},\code{"loglogistic"},\code{"lognormal"}.}

\item{method}{The method used to do the estimation. Current possible
values are \code{"mle"}.}

\item{weights}{Optional for case weights. The function expects a string
corresponding to a variable name within the data.}

\item{...}{Additional arguments for \code{runMLE.cure}.}
}
\value{
A list containing the fitted parametric mixture cure survival models for
each specified distribution.
The list includes the model objects and additional information such as AIC,
BIC, and KM data. This object is analogous to the object generated from the
\code{\link[survHE]{fit.models}} function, but with an added cure fraction.
}
\description{
\code{fit.models.cure} fits parametric mixture cure survival models
for health economic evaluations.The code for \code{fit.models.cure} is primarily
taken from the equivalent DARTH function, with minor edits to functionality
and documentation.
}
\examples{
\dontrun{
library(survival)

fit <- fit.models.cure(Surv(time, status) ~ as.factor(sex),
  data = lung,
  distr = c("exponential", "weibull"), method = "mle"
)
print(fit)
}

}
