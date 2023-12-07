#' Fit spline models in survival analysis for health economic evaluations
#'
#' This function fits spline models specified by different knot configurations
#' to survival data.
#'
#' @param formula A formula specifying the model to be used, in the form
#' \code{Surv(time, event) ~ strata[+covariates]} in flexsurv terms.
#' @param dists A data frame with columns \code{knots} and \code{scale}
#' specifying the number of knots to be used and the type of spline model to be
#' executed respectively. The possible models are: Proportional hazards model
#' (\code{scale = "hazard"}), probit model (\code{scale = "normal"}),
#' proportional odds model (\code{scale = "odds"}).
#' @param data A data frame containing the data to be used for the analysis.
#' This must contain data for the \code{time} and \code{event} variables.
#' @param method Method from survHE, default is mle.
#' @param weights Optional for case weights. The function expects a string
#' corresponding to a variable name within the data.
#'
#' @importFrom survHE fit.models
#' @export
#'
#' @return A list containing the fitted spline models and model fitting
#' information.
#'
#' @examples
#' \dontrun{
#' library(survival)
#'
#' formula <- Surv(time, status) ~ as.factor(sex)
#' dists <- data.frame(knots = c(3, 5), scale = rep("normal", 2))
#' spline_fit <- fit_splines(formula, dists, data = lung)
#' print(spline_fit)
#' }
#'
fit_splines <- function(formula, dists, data, method = "mle", weights = NULL) {

  # lapply not possible as survHE forces knots to be single integer.

  for (dist_num in seq_len(nrow(dists))) {
    fitting <- survHE::fit.models(
      formula = formula,
      data = data,
      k = dists$knots[dist_num],
      scale = dists$scale[dist_num],
      distr = "splines", method = method,
      weights = weights
    )
    names(fitting$models) <- paste(
      fitting$models[[1]]$call$k,
      "knot",
      fitting$models[[1]]$call$scale
    )

    # Acknowledging limitations of append, this is the most convenient way
    # to group the spline fit objects in a survHE style.

    if (dist_num == 1) {
      out <- fitting
    } else {
      out$models <- append(out$models, fitting$models)
      out$misc$model_name[dist_num] <- "rps"
      out$model.fitting$aic[dist_num] <- fitting$model.fitting$aic[1]
      out$model.fitting$bic[dist_num] <- fitting$model.fitting$bic[1]
    }
  }
  return(out)
}
