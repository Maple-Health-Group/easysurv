###########
# This file WAS CREATED for the new easysurv release.
###########

#' @importFrom data.table rbindlist
#' @importFrom dplyr rename_with
#'
#' @export
get_fit_averages <- function(mod,
                              get_median = TRUE,
                              get_rmst = TRUE,
                              get_mean = FALSE) {
  if (!get_median & !get_rmst & !get_mean) {
    stop("You need to include at least one average (median, rmst, or mean) in the get_fit_averages function")
  }

  out <- list()

  engine <- mod$spec$engine

  if (engine == "flexsurv" | engine == "flexsurvcure" | engine == "flexsurvspline") {
    # Checking if the distribution is a spline model.
    distribution <- `if`(is.null(mod$fit$k), mod$fit$dlist$name, paste(
      mod$fit$k,
      "knot",
      mod$fit$scale
    ))

    # MEDIAN
    if (get_median) {
      median <- tryCatch(
        {
          summary(mod$fit, type = "median")
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
          summary(mod$fit, type = "rmst")
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
          summary(mod$fit, type = "mean")
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



    # Combine into list per strata
    for (i in seq_along(myseq)) {
      # out[[i]] <- distribution
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
  } else if (engine == "survival") {
    distribution <- mod$fit$dist

    # Check for groups
    if (!is.null(mod$fit$xlevels)) {
      n_xlevels <- length(mod$fit$xlevels)

      for (i in seq_along(n_xlevels)) {
        out[[i]] <- data.frame(distribution = distribution)

        # Create a single row data frame for prediction data
        new_data <- data.frame(column1 = mod$fit$xlevels[[1]][i])
        # Rename the column to match that in the original data
        colnames(new_data) <- names(mod$fit$xlevels)

        # Get the median
        median[[i]] <- predict(mod$fit,
                               newdata = new_data,
                               type = "quantile",
                               p = c(0.5)
        )

        out[[i]] <- cbind(out[[i]], median[[i]])
      }

      out <- data.table::rbindlist(out)
    } else {
      out <- data.frame(distribution = distribution)

      # Get the median (newdata does not matter)
      median <- predict(mod$fit,
                        newdata = data.frame(testing = 123),
                        type = "quantile",
                        p = c(0.5)
      )

      out <- cbind(out, median)
    }
  }

  return(out)
}
