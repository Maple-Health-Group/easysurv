#' Check whether survival models extrapolations converge for survival analysis
#'
#' This function checks whether survival model extrapolations converge by
#' fitting the models using the \code{\link[survHE]{fit.models}} function and
#' checking for convergence. Extrapolations that do not converge are excluded
#' from the output.
#'
#'
#' @param formula a formula specifying the model to be used, in the form of
#' \code{Surv(time,event)~strata[+covariates]} in /code{flexsurv} terms.
#' @param data A data frame containing the data to be used for the analysis.
#' This must contain data for the \code{time} and \code{event} variables.
#' @param dists Name or vector of names for the distribution(s) to be used.
#'
#' @importFrom survHE fit.models
#' @export
#'
#' @return A vector of distribution names that have converged.
#'
#' @examples
#' \dontrun{
#' library(survival)
#'
#' check_converged(
#'   formula = Surv(time, status) ~ as.factor(sex),
#'   data = lung[1:7,],
#'   dists = c("exponential", "weibull", "gengamma", "lognormal")
#' )
#' }
#'
check_converged <- function(formula, data, dists) {

  # Define a function to check convergence
  check_convergence <- function(dist_name) {
    tryCatch(
      {
        survHE::fit.models(
          formula = formula,
          data = data,
          distr = dist_name,
          method = "mle"
        )
        return(NULL)  # No error or warning
      },
      error = function(e) {
        message(paste(dist_name, "did not converge and was removed."))
        return(dist_name)
      },
      warning = function(w) {
        message(paste(dist_name, "may not have converged and was removed."))
        return(dist_name)
      }
    )
  }

  # Use lapply to check convergence for each distribution
  failed_dists <- unlist(lapply(dists, check_convergence))

  # Remove distributions that failed to converge
  out_dists <- setdiff(dists, failed_dists)

  return(out_dists)
}
