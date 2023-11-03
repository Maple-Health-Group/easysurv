#' Check whether spline models converge
#'
#' This function checks whether spline models specified by different knot
#' configurations converge.
#'
#' @param formula A formula specifying the model to be used, in the form
#' \code{Surv(time, event) ~ strata[+covariates]} in flexsurv terms.
#' @param data A data frame containing the data to be used for the analysis.
#' This must contain data for the \code{time} and \code{event} variables.
#' @param dists A data frame with columns \code{knots} and \code{scale}
#' specifying the number of knots to be used and the type of spline model to be
#' executed respectively. The possible models are: Proportional hazards model
#' (\code{scale = "hazard"}), probit model (\code{scale = "normal"}),
#' proportional odds model (\code{scale = "odds"}).
#'
#' @importFrom survHE fit.models
#' @export
#'
#' @return A modified version of the \code{dists} input dataframe after
#' removing the models that did not converge.
#'
#' @examples
#' \dontrun{
#' library(survival)
#'
#' formula <- Surv(time, status) ~ as.factor(sex)
#' dists <- data.frame(knots = c(3, 5),
#'                     scale = rep("normal", 2))
#' check_converged_splines(formula, data = lung[1:9,], dists)
#' }
#'
check_converged_splines <- function(formula, data, dists) {
  n <- nrow(dists)
  keep_rows <- vector("logical", length = n)

  for (dist_num in seq_len(n)) {
    tryCatch(
      {
        result <- survHE::fit.models(
          formula = formula,
          data = data,
          distr = "splines",
          k = dists$knots[dist_num],
          scale = dists$scale[dist_num],
          method = "mle"
        )
        keep_rows[dist_num] <- TRUE
      },
      error = function(e) {
        message(paste(
          dists$knots[dist_num],
          "knots",
          dists$scale[dist_num],
          "model did not converge and was removed."
        ))
      },
      warning = function(w) {
        message(paste(
          dists$knots[dist_num],
          "knots",
          dists$scale[dist_num],
          "model may not have converged and was removed."
        ))
      }
    )
  }

  return(dists[keep_rows, ])
}
