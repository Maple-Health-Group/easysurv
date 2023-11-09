#' Obtain median, mean, and restricted mean survival times.
#'
#' This function obtains the median, mean, and restricted mean survival times
#' stored in a \code{flexsurvreg} or \code{\link[survHE]{fit.models}} object.
#'
#' @param mod A specific model of a \code{fit.models} object obtained from
#' the \code{\link[survHE]{fit.models}} function.
#'
#' @importFrom dplyr rename_with mutate
#' @export
#'
#' @return A data frame containing the median, mean and restricted mean survival
#' times (with 95% confidence intervals) where calculable.
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
#' lapply(fit$models, get_fit_averages)
#'
#' # Example using a flexsurv object
#' fit <- flexsurvreg(Surv(time, status) ~ as.factor(sex),
#'   data = lung,
#'   dist = "exponential"
#' )
#' get_fit_averages(fit)
#' }
get_fit_averages <- function(mod) {

  # Checking if the distribution is a spline model.
  distribution <- `if`(is.null(mod$k), mod$dlist$name, paste(
      mod$k,
      "knot",
      mod$scale
  ))

  # MEDIAN
  median <- tryCatch(
    {
      summary(mod, type = "median")
    },
    error = function(e) {
      message(paste(distribution, "median survival time not calculable."))
      return(list(as.data.frame(list("est" = "-", "lcl" = "-", "ucl" = "-"))))
    }
  )

  # RESTRICTED MEAN
  restricted_mean <- tryCatch(
    {
      summary(mod, type = "rmst")
    },
    error = function(e) {
      message(paste(distribution, "restricted mean survival time not calculable."))
      return(list(as.data.frame(list("est" = "-", "lcl" = "-", "ucl" = "-"))))
    }
  )

  # MEAN (most likely to fail due to Gompertz and Inf)
  mean <- tryCatch(
    {
      summary(mod, type = "mean")
    },
    error = function(e) {
      message(paste(distribution, "mean survival time not calculable."))
      return(list(as.data.frame(list("est" = "-", "lcl" = "-", "ucl" = "-"))))
    }
  )

  # In case of strata, use seq_along
  for (i in seq_along(median)) {
    median[[i]] <- median[[i]] |>
      dplyr::rename_with( ~ paste0("median.", .x))

    restricted_mean[[i]] <- restricted_mean[[i]] |>
      dplyr::rename_with( ~ paste0("rmst.", .x))

    mean[[i]] <- mean[[i]] |>
      dplyr::rename_with( ~ paste0("mean.", .x))
  }

  out <- list()

  # Combine into list per strata
  for (i in seq_along(median)) {

    if (is.null(names(median)[i])) {
      out[[i]] <- cbind(distribution,
                        median[[i]],
                        restricted_mean[[i]],
                        mean[[i]])
    } else {
      strata <- names(median)[i]
      out[[i]] <- cbind(distribution,
                        strata,
                        median[[i]],
                        restricted_mean[[i]],
                        mean[[i]])
    }
  }

  names(out) <- names(median)

  out <- data.table::rbindlist(out)

  return(out)
}
