#' Assign names as v1, v2, v3 to variance covariance matrices
#'
#' This function assigns names as v1, v2, v3, etc., to the variance-covariance
#' matrices obtained from a
#' \code{\link[flexsurv]{flexsurvreg}} or
#' \code{\link[flexsurvcure]{flexsurvcure}} object,which are listed in the
#' output of the \code{\link[survHE]{fit.models}} function under \code{$models}.
#' The flexsurv object can alternately be created using
#' \code{\link[flexsurv]{flexsurvreg}} or
#' \code{\link[flexsurvcure]{flexsurvcure}}.
#' It is intended to facilitate tabulated output for applications such as
#' exporting to Excel.
#'
#' @param mod An object of class \code{flexsurv} obtained from the
#' \code{\link[survHE]{fit.models}}, \code{\link[flexsurv]{flexsurvreg}} or
#' \code{\link[flexsurvcure]{flexsurvcure}} functions.
#' If \code{mod} is obtained from \code{\link[survHE]{fit.models}},
#' must specify the \code{$models} element of the object.Ensure that, in the
#' formula used to make
#' \code{mod} (\code{Surv(time,event)~strata[+covariates]}),
#' \code{class(strata) == "factor"}.
#'
#' @importFrom stats vcov
#' @export
#'
#' @return A data frame representing the variance-covariance matrix with
#' named columns. The rows correspond to the variables or coefficients, and
#' the columns are named as v1, v2, v3, etc., denoting the variance-covariance
#' elements.
#'
#' @examples
#' \dontrun{
#' library(survHE)
#'
#' # Example usage with a fit.models object
#' fit <- fit.models(Surv(time, status) ~ as.factor(sex),
#'   data = lung, distr = c("exponential", "weibull")
#' )
#' lapply(fit$models, get_fit_vcov_names)
#'
#' # Example using a flexsurv object
#' fit <- flexsurvreg(Surv(time, status) ~ as.factor(sex), data = lung,
#' dist = "exponential")
#' get_fit_vcov_names(fit)
#' }
get_fit_vcov_names <- function(mod) {

  if(!is.null(mod$coefficients)){
    vcov_matrix <- as.data.frame(stats::vcov(mod))

    # Making the column names consistent between models
    colnames(vcov_matrix) <- paste0("v", seq_along(mod$coefficients))

    # Add a column to mark covariates
    vcov_matrix$cov_marker <- NA

    if (!is.null(mod$covpars)) {
      vcov_matrix$cov_marker[mod$covpars] <- "covariate"
    }

    return(vcov_matrix)

  } else {
    stop(paste0(
      "No model coefficients detected in function get_fit_vcov names.",
      "\n",
      "Cannot compute vcov."))
  }

}
