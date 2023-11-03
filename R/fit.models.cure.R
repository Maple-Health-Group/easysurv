#' Fit parametric mixture cure survival models for health economic evaluations
#'
#' \code{fit.models.cure} fits parametric mixture cure survival models
#' for health economic evaluations.
#' Most code here is lifted from
#' \url{github.com/DARTH-git/darthtools/blob/main/R/survival_functions.R}
#' at 12 June 2023 - and then minor edits were made, mostly to documentation.
#'
#' @param formula A formula specifying the model to be used, in the form
#' \code{Surv(time,event)~strata[+covariates]} for /code{flexsurv}. Recommend
#' that \code{class(strata) == "factor"}.
#' @param data A data frame containing the data to be used for the analysis.
#' This must contain data for the 'event' variable.
#' @param distr A (vector of) string(s) containing the name(s) of the model(s)
#' to be fitted. Available options are: \code{"exponential"},\code{"gamma"},
#' \code{"genf"}, \code{"gengamma"},\code{"gompertz"},\code{"weibull"},
#' \code{"weibullPH"},\code{"loglogistic"},\code{"lognormal"}.
#' @param method The method used to do the estimation. Current possible
#' values are \code{"mle"}.
#' @param ... Additional arguments for \code{runMLE.cure}.
#'
#' @return
#' A list containing the fitted parametric mixture cure survival models for
#' each specified distribution.
#' The list includes the model objects and additional information such as AIC,
#' BIC, and KM data. This object is analogous to the object generated from the
#' \code{\link[survHE]{fit.models}} function, but with an added cure fraction.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(survival)
#'
#' fit <- fit.models.cure(Surv(time, status) ~ as.factor(sex),
#'   data = lung,
#'   distr = c("exponential", "weibull"), method = "mle"
#' )
#' print(fit)
#' }
#'
fit.models.cure <- function(formula = NULL, data, distr = NULL, method = "mle",
                            ...) {
  exArgs <- list(...)
  exArgs$formula <- formula
  exArgs$data <- data
  if (is.null(formula)) {
    stop("You need to specify a model 'formula',
         e.g. 'formula=Surv(time,event)~1'")
  }
  method <- tolower(method)
  if (!method %in% c("mle")) {
    stop("Methods available for use are 'mle'")
  }
  check_distributions(method, distr)
  if (method == "mle") {
    res <- format_output_fit.models(lapply(distr, function(x) {
      runMLE.cure(
        x,
        exArgs
      )
    }), method, distr, formula, data)
  }
  return(res)
}
