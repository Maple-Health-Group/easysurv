#' Get the variance-covariance matrix from a fit.models object
#'
#' This function retrieves the variance-covariance matrix from a
#' \code{\link[flexsurv]{flexsurvreg}} or
#' \code{\link[flexsurvcure]{flexsurvcure}} object, which are listed in the
#' output of the \code{\link[survHE]{fit.models}} function under \code{$models}.
#' The flexsurv object can alternately be created using
#' \code{\link[flexsurv]{flexsurvreg}} or
#' \code{\link[flexsurvcure]{flexsurvcure}}.
#' The variance-covariance matrix provides information about the estimated
#' variances and covariances of the model parameters.
#'
#' @param mod An object of class \code{flexsurv} obtained from the
#' \code{\link[survHE]{fit.models}}, \code{\link[flexsurv]{flexsurvreg}} or
#' \code{\link[flexsurvcure]{flexsurvcure}} functions.
#'
#' @importFrom stats vcov
#'
#' @export
#'
#' @return A data frame containing the variance-covariance matrix of the
#' model parameters. The data frame has column names corresponding to the
#' parameters and row names representing the parameter pairs. The values in
#' the matrix represent the estimated variances and covariances.
#'
#' @examples
#' \dontrun{
#' library(survHE)
#'
#' # Example usage with a fit.models object
#' fit <- fit.models(Surv(time, status) ~ as.factor(sex),
#'   data = lung, distr = c("exponential", "weibull")
#' )
#' lapply(fit, get_fit_vcov)
#'
#' # Example using a flexsurv object
#' fit <- flexsurvreg(Surv(time, status) ~ as.factor(sex), data = lung,
#' dist = "exponential")
#' get_fit_vcov(fit)
#' }
#'
get_fit_vcov <- function(mod) {
  stats::vcov(mod) |>
    as.data.frame()
}
