#' Obtain median, mean, and restricted mean survival times.
#'
#' This function obtains the median, mean, and restricted mean survival times
#' stored in a \code{flexsurvreg} or \code{\link[survHE]{fit.models}} object.
#'
#' @param mod An object of class \code{fit.models} obtained from the
#' \code{\link[survHE]{fit.models}} function.
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
#' lapply(fit$models, get_fit_average)
#'
#' # Example using a flexsurv object
#' fit <- flexsurvreg(Surv(time, status) ~ as.factor(sex),
#'   data = lung,
#'   dist = "exponential"
#' )
#' get_fit_average(fit)
#' }
get_fit_average <- function(mod) {

  median <- summary(mod, type = "median")

  # checking if the model is splines
  the_dist <- `if`(is.null(mod$k), mod$dlist$name, paste(
      mod$k,
      "knot",
      mod$scale
  ))

  # Handling "Inf" errors when calculating the mean
  mean <- tryCatch(
    {
      summary(mod, type = "mean")
    },
    error = function(e) {
      message(paste(the_dist, "mean survival time not calculable."))
      return(list(as.data.frame(list("est" = "-", "lcl" = "-", "ucl" = "-"))))
    }
  )

  restricted_mean <- summary(mod, type = "rmst")

  # In case of strata, use seq_along

  for (i in seq_along(median)) {
    median[[i]] <- median[[i]] |>
      dplyr::rename_with( ~ paste0("median.", .x))
  }

  for (i in seq_along(mean)) {
    mean[[i]] <- mean[[i]] |>
      dplyr::rename_with( ~ paste0("mean.", .x))
  }

  for (i in seq_along(restricted_mean)) {
    restricted_mean[[i]] <- restricted_mean[[i]] |>
      dplyr::rename_with( ~ paste0("rmst.", .x))
  }

  out <- list()

  # Combine into list per strata
  for (i in seq_along(median)) {
    model <- the_dist
    strata <- `if`(is.null(names(median)[i]),"-", names(median)[i])
    out[[i]] <- cbind(model, strata, median[[i]], mean[[i]], restricted_mean[[i]])
  }

  names(out) <- names(median)

  out <- data.table::rbindlist(out)

  #out <- do.call(rbind, Map(data.frame, out))
  #out <- bind_rows(out, .id = "column_label")

  return(out)
}
