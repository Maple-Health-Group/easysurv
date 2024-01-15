#' Generate a data frame of predicted survival from a fit.models or
#' fit.models.cure object
#'
#' This function generates a data frame of predicted survival values based on a
#' \code{\link[survHE]{fit.models}} or \code{\link{fit.models.cure}} object.
#' It takes a set of times and the group in the data, and returns a data frame
#' with predicted survival values for each model in the input object at the
#' specified times.
#'
#' @param fits An object obtained from \code{\link[survHE]{fit.models}} or
#' \code{\link{fit.models.cure}}.
#' @param t A vector of times at which to calculate predicted survival,
#' e.g., \code{seq(from = 0, to = 1000, length.out = 100)}
#' @param group In cases of multiple stratas within one
#' \code{\link[survHE]{fit.models}} object, the index of the "strata" of
#' interest (e.g., In data frames with two treatments, \code{group = "1"} or
#' \code{group = "2"})
#' @param ci Logical indicating whether to include confidence intervals in the
#' output.
#'
#' @export
#'
#' @importFrom tibble as_tibble
#'
#' @return A data frame with predicted survival values for each model at the
#' specified times.
#'
#' @examples
#' \dontrun{
#' library(survHE)
#'
#' # Single-arm
#' fits <- fit.models(Surv(time, status) ~ 1,
#'   data = lung,
#'   dist = c("exponential", "weibull")
#' )
#' t <- seq(from = 0, to = 1000, length.out = 100)
#' predict_fits(fits, t, group = 1)
#'
#' # Double-arm
#' fits <- fit.models(Surv(time, status) ~ as.factor(sex),
#'   data = lung,
#'   dist = c("exponential", "weibull")
#' )
#' t <- seq(from = 0, to = 1000, length.out = 100)
#' predict_fits(fits, t, group = 1)
#' predict_fits(fits, t, group = 2)
#' }
#'
predict_fits <- function(fits, t, group = 1, ci = TRUE) {

  # Pre-allocation for speed
  num_times <- length(t)
  num_models <- length(fits$models)

  predicts <- matrix(NA,
                     nrow = num_times,
                     ncol = num_models + 1)

  colnames(predicts) <- c("t", names(fits$models))

  # Create objects to store predictions
  predicts[, 1] <- t
  low <- predicts
  upp <- predicts

  # Compute survival probabilities for each models
  for (i in seq_len(num_models)) {
    dist <- fits$models[[i]]
    pred_surv <- summary(dist, type = "survival", t = t, ci = TRUE)[[group]]
    predicts[, i + 1] <- as.matrix(pred_surv[2])
    low[, i + 1] <- as.matrix(pred_surv[3])
    upp[, i + 1] <- as.matrix(pred_surv[4])
  }

  # Output is a list object, including lower and upper CI values, if ci is enabled, otherwise it is a data.frame.
  if (ci) {
    out <- list(tibble::as_tibble(predicts),
                tibble::as_tibble(low),
                tibble::as_tibble(upp))

    names(out) <- c("predictions", "lower", "upper")
  } else {
    out <- tibble::as_tibble(predicts)
  }

  return(out)

}
