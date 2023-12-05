#' Obtain median, restricted mean, and mean survival times.
#'
#' This function can obtain the median, restricted mean and mean survival times
#' stored in a \code{flexsurvreg} or \code{\link[survHE]{fit.models}} object.
#' By default, mean times are not returned as certain distributions may plateau,
#' which makes calculation not possible.
#'
#' @param mod A specific model of a \code{fit.models} object obtained from
#' the \code{\link[survHE]{fit.models}} function.
#' @param get_median Optional for whether to return median survival times.
#' Defaults to TRUE.
#' @param get_rmst Optional for whether to return restricted mean survival
#' times. Defaults to TRUE.
#' @param get_mean Optional for whether to return mean survival times.
#' Defaults to FALSE.
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
get_fit_averages <- function(mod,
                             get_median = TRUE,
                             get_rmst = TRUE,
                             get_mean = FALSE) {
  if (!get_median & !get_rmst & !get_mean) {
    stop("You need to include at least one average (median, rmst, or mean) in the get_fit_averages function")
  }

  # Checking if the distribution is a spline model.
  distribution <- `if`(is.null(mod$k), mod$dlist$name, paste(
    mod$k,
    "knot",
    mod$scale
  ))

  # MEDIAN
  if (get_median) {
    median <- tryCatch(
      {
        summary(mod, type = "median")
      },
      error = function(e) {
        message(paste(
          "warning:",
          distribution,
          "median survival time not calculable."
        ))
        return(list(as.data.frame(list("est" = "-", "lcl" = "-", "ucl" = "-"))))
      }
    )
  }

  # RESTRICTED MEAN
  if (get_rmst) {
    restricted_mean <- tryCatch(
      {
        summary(mod, type = "rmst")
      },
      error = function(e) {
        message(paste(
          "warning:",
          distribution,
          "restricted mean survival time not calculable."
        ))
        return(list(as.data.frame(list("est" = "-", "lcl" = "-", "ucl" = "-"))))
      }
    )
  }

  # MEAN (most likely to fail)
  if (get_mean) {
    mean <- tryCatch(
      {
        summary(mod, type = "mean")
      },
      error = function(e) {
        message(paste(
          "warning:",
          distribution,
          "mean survival time not calculable",
          "(likely due to a plateau in survival predictions)"
        ))
        return(list(as.data.frame(list("est" = "-", "lcl" = "-", "ucl" = "-"))))
      }
    )
  }

  if (get_median) {
    myseq <- median
  } else if (get_rmst) {
    myseq <- restricted_mean
  } else {
    myseq <- mean
  }

  # In case of strata, use seq_along
  for (i in seq_along(myseq)) {
    if (get_median) {
      median[[i]] <- median[[i]] |>
        dplyr::rename_with(~ paste0("median.", .x))
    }

    if (get_rmst) {
      restricted_mean[[i]] <- restricted_mean[[i]] |>
        dplyr::rename_with(~ paste0("rmst.", .x))
    }

    if (get_mean) {
      mean[[i]] <- mean[[i]] |>
        dplyr::rename_with(~ paste0("mean.", .x))
    }
  }

  out <- list()

  # Combine into list per strata
  for (i in seq_along(myseq)) {

    #out[[i]] <- distribution
    out[[i]] <- data.frame(distribution = distribution)

    if (!is.null(names(myseq)[i])) {
      strata <- names(myseq)[i]
      out[[i]] <- cbind(
        out[[i]],
        strata
      )
    }

    if (get_median) {
      out[[i]] <- cbind(
        out[[i]],
        median[[i]]
      )
    }

    if (get_rmst) {
      out[[i]] <- cbind(
        out[[i]],
        restricted_mean[[i]]
      )
    }

    if (get_mean) {
      out[[i]] <- cbind(
        out[[i]],
        mean[[i]]
      )
    }
  }

  names(out) <- names(myseq)

  out <- data.table::rbindlist(out)

  return(out)
}
