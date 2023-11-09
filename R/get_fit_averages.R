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
#' @return A data frame containing the comparison of goodness-of-fit measures
#' for the models. The data frame has columns for the model names, AIC values,
#' BIC values, AIC ranks, and BIC ranks.
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
  the_dist <- `if`(is.null(mod$k), mod$dlist$name, paste(
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
      message(paste(the_dist, "median survival time not calculable."))
      return(list(as.data.frame(list("est" = "-", "lcl" = "-", "ucl" = "-"))))
    }
  )

  # MEAN
  mean <- tryCatch(
    {
      summary(mod, type = "mean")
    },
    error = function(e) {
      message(paste(the_dist, "mean survival time not calculable."))
      return(list(as.data.frame(list("est" = "-", "lcl" = "-", "ucl" = "-"))))
    }
  )

  # RESTRICTED MEAN
  restricted_mean <- tryCatch(
    {
      summary(mod, type = "rmst")
    },
    error = function(e) {
      message(paste(the_dist, "restricted mean survival time not calculable."))
      return(list(as.data.frame(list("est" = "-", "lcl" = "-", "ucl" = "-"))))
    }
  )

  # In case of strata, use seq_along
  for (i in seq_along(median)) {
    median[[i]] <- median[[i]] |>
      dplyr::rename_with( ~ paste0("median.", .x))

    mean[[i]] <- mean[[i]] |>
      dplyr::rename_with( ~ paste0("mean.", .x))

    restricted_mean[[i]] <- restricted_mean[[i]] |>
      dplyr::rename_with( ~ paste0("rmst.", .x))
  }

  out <- list()

  # Combine into list per strata
  for (i in seq_along(median)) {
    strata <- `if`(is.null(names(median)[i]),"-", names(median)[i])
    out[[i]] <- cbind(the_dist, strata, median[[i]], mean[[i]], restricted_mean[[i]])
  }

  names(out) <- names(median)

  out <- data.table::rbindlist(out)

  return(out)
}
