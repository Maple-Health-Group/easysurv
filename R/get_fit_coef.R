#' Get parameter estimates from a fit.models object
#'
#' This function retrieves the parameter estimates from a
#' \code{\link[flexsurv]{flexsurvreg}} or
#' \code{\link[flexsurvcure]{flexsurvcure}} object,
#' which are listed in the output of the \code{\link[survHE]{fit.models}}
#' function under \code{$models}.
#' The flexsurv object can alternately be created using
#' \code{\link[flexsurv]{flexsurvreg}} or
#' \code{\link[flexsurvcure]{flexsurvcure}}.
#'
#' @param mod An object of class \code{flexsurv} obtained from
#' the\code{\link[survHE]{fit.models}}, \code{\link[flexsurv]{flexsurvreg}}
#' or \code{\link[flexsurvcure]{flexsurvcure}} functions.
#'
#' @param method The method used to estimate the parameters. The default is
#' \code{'mle'} which corresponds to maximum likelihood estimation.
#' Alternatively, if \code{'hmc'} is specified, the function assumes
#' the model was fitted using Hamiltonian Monte Carlo (HMC) via the
#' \code{`rstan`} package. In this case, the parameter estimates are obtained
#' from the HMC model. This is not imported intentionally.
#'
#' @importFrom tibble rownames_to_column
#'
#' @export
#'
#' @return A data frame containing the parameter estimates from the
#' fitted models. The data frame has two columns: \code{parameter} and
#' \code{Coef}. The \code{parameter} column lists the names of the model
#' parameters, and the \code{Coef} column contains their corresponding
#' estimated values.
#'
#' @examples
#' \dontrun{
#' library(survHE)
#' library(flexsurv)
#'
#' # Example usage with a fit.models object
#' fit <- fit.models(Surv(time, status) ~ as.factor(sex),
#'   data = lung,
#'   distr = c("exponential", "weibull")
#' )
#' lapply(fit$models, get_fit_coef)
#'
#' # Example using a flexsurv object
#' fit <- flexsurvreg(Surv(time, status) ~ as.factor(sex),
#'   data = lung,
#'   dist = "exponential"
#' )
#' get_fit_coef(fit)
#'
#' # Example usage with a fit.models object fitted using HMC
#' fit_hmc <- fit.models(Surv(time, status) ~ as.factor(sex),
#'   data = lung,
#'   distr = c("exponential", "weibull"), method = "hmc"
#' )
#' lapply(fit_hmc, get_fit_coef)
#' }
get_fit_coef <- function(mod, method = "mle") {
  if (method == "mle") {
    # get parameter estimates from a flexsurv object
    par.est <- mod$res.t |>
      as.data.frame() |>
      tibble::rownames_to_column(var = "parameter")
  }
  if (method == "hmc") {
    # get parameter estimates from a model fitted with HMC via stan
    if (requireNamespace("rstan", quietly = TRUE)) {
      par.est <- rstan::summary(mod)$summary |>
        as.data.frame() |>
        tibble::rownames_to_column(var = "parameter")
    } else {
      stop("Package \"rstan\" needed for models fitted with HMC.
      Please install it.")
    }
  }
  colnames(par.est)[2] <- "Coef"
  return(par.est)
}
