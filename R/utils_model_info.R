# Non-exported helper functions.

#' @importFrom stats pnorm
#' @noRd
get_cure_fractions <- function(mod) {

  # Define the link function
  link_function <- mod$fit$link

  # Get theta
  theta <- mod$fit$res.t[1]

  # Calculate cure fraction based on the link function
  cure_fraction <- switch(
    link_function,
    logistic = exp(theta) / (1 + exp(theta)),
    loglog = exp(-exp(theta)),
    probit = stats::pnorm(theta),
    identity = max(0, min(1, theta)),
    stop("Unknown link function")
  )

  return(cure_fraction)

}

#' @importFrom data.table rbindlist
#' @importFrom dplyr rename_with
#' @noRd
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

#' @importFrom data.table rbindlist
#' @importFrom tibble as_tibble
#' @noRd
get_fit_averages_summary <- function(models,
                                     get_median = TRUE,
                                     get_rmst = TRUE,
                                     get_mean = FALSE) {
  out <- lapply(models,
                get_fit_averages,
                get_median = get_median,
                get_rmst = get_rmst,
                get_mean = get_mean
  )

  out <- data.table::rbindlist(out) |> tibble::as_tibble()

  return(out)
}

#' @importFrom stats AIC BIC
#' @importFrom tibble tibble
#' @noRd
get_goodness_of_fit <- function(mod) {
  # Get AIC and BIC values using stats:: because engine=survival doesn't record these
  AIC_values <- sapply(mod, function(x) stats::AIC(x$fit))
  BIC_values <- sapply(mod, function(x) stats::BIC(x$fit))

  AIC_ranks <- rank(AIC_values)
  BIC_ranks <- rank(BIC_values)

  out <- tibble::tibble(
    "dist" = names(mod),
    "AIC" = AIC_values,
    "BIC" = BIC_values,
    "AIC_rank" = AIC_ranks,
    "BIC_rank" = BIC_ranks
  )

  out[order(out$dist), ]
}

#' @importFrom dplyr bind_rows relocate
#' @importFrom tibble as_tibble rownames_to_column
#' @importFrom utils tail
#' @noRd
get_surv_parameters <- function(models) {
  # Initialize an empty list to store results
  out <- list()

  for (i in seq_along(models)) {
    engine <- models[[i]]$spec$engine

    distribution <- switch(engine,
                           "flexsurv" = models[[i]]$fit$dlist$name,
                           "flexsurvcure" = models[[i]]$fit$dlist$name,
                           "flexsurvspline" = names(models[i]),
                           "survival" = models[[i]]$fit$dist,
                           stop("Unknown engine type")
    )

    if (engine == "flexsurv" | engine == "flexsurvcure" | engine == "flexsurvspline") {
      # Get parameters from res.t
      get_parameters <- models[[i]]$fit$res.t |>
        as.data.frame() |>
        tibble::rownames_to_column(var = "parameter")

      # Get vcov matrix
      get_vcov <- models[[i]]$fit$cov |>
        as.data.frame()

      # Make the column names consistent between models (v1, v2, v3, ...)
      colnames(get_vcov) <- paste0("v", seq_along(models[[i]]$fit$coefficients))

      # Combine all results
      combined_results <- cbind(distribution, get_parameters, get_vcov)

      # Track location for covariate parameter
      combined_results$covariate_marker <- combined_results$parameter
      if (!is.null(models[[i]]$fit$covpars)) {
        combined_results$covariate_marker[models[[i]]$fit$covpars] <- models[[i]]$fit$dlist$location
      }
    } else if (engine == "survival") {
      # With the survival package, it's a bit tricky.
      # Get number of parameters using degrees of freedom
      par_length <- length(models[[i]]$fit$df)

      # Get initial parameters from coefficients
      get_parameters <- models[[i]]$fit$coefficients |>
        as.data.frame() |>
        tibble::rownames_to_column(var = "parameter")
      colnames(get_parameters) <- c("parameter", "est")

      # If there's an additional parameter not included in coefficients, add it
      if (utils::tail(names(models[[i]]$fit$icoef), 1) != utils::tail(names(models[[i]]$fit$coefficients), 1)) {
        get_additional_parameters <- utils::tail(models[[i]]$fit$icoef, 1) |>
          as.data.frame() |>
          tibble::rownames_to_column(var = "parameter")
        colnames(get_additional_parameters) <- c("parameter", "est")

        get_parameters <- rbind(get_parameters, get_additional_parameters)
      }

      # Get vcov matrix
      get_vcov <- models[[i]]$fit$var |>
        as.data.frame()

      # Make the column names consistent between models (v1, v2, v3, ...)
      colnames(get_vcov) <- paste0("v", seq_along(get_parameters$parameter))

      # Combine all results
      combined_results <- cbind(distribution, get_parameters, get_vcov)

      # Track location for covariate parameter (not reported in survival output)
      combined_results$covariate_marker <- "NR"
    }

    # Variance-covariance matrices
    out[[i]] <- combined_results
  }

  # Start the tibble with distribution, parameter and covariate_marker columns.
  out <- dplyr::bind_rows(out) |>
    dplyr::relocate("distribution", "parameter", "covariate_marker") |>
    tibble::as_tibble()

  return(out)
}

#' @importFrom purrr map
#' @importFrom dplyr slice
#' @importFrom tibble as_tibble
#' @importFrom tidyr unnest
#' @importFrom stats predict
#' @noRd
tidy_predict_surv <- function(models,
                              new_data,
                              eval_time,
                              interval = "none") {

  # Start with NULLs to make dropping them easy with c()
  list_pred_surv <-
    list_pred_hazard <-
    table_pred_surv <-
    table_pred_surv_lower <-
    table_pred_surv_upper <-
    table_pred_hazard <- NULL

  # make the predictions (survival)
  list_pred_surv <- lapply(models,
                           stats::predict,
                           new_data = new_data,
                           type = "survival",
                           eval_time = eval_time,
                           interval = interval
  ) |>
    purrr::map(~ .x |>
                 dplyr::slice(1) |>
                 tidyr::unnest(col = .pred))

  # inner function to extract predictions into a table
  extract_predictions <- function(pred_list, col_name) {
    Reduce(
      function(x, y) merge(x, y, by = ".eval_time", all = TRUE),
      (lapply(names(pred_list), function(model) {
        df <- pred_list[[model]][, c(".eval_time", col_name)]
        colnames(df)[2] <- model
        return(df)
      }))
    ) |> tibble::as_tibble()
  }

  # Extract to summary tables
  table_pred_surv <- extract_predictions(list_pred_surv, ".pred_survival")
  if (interval == "confidence" & models[[1]]$spec$engine != "survival") {
    table_pred_surv_lower <- extract_predictions(list_pred_surv, ".pred_lower")
    table_pred_surv_upper <- extract_predictions(list_pred_surv, ".pred_upper")
  }


  # make the predictions (hazard)
  list_pred_hazard <- lapply(models,
                             stats::predict,
                             new_data = new_data,
                             type = "hazard",
                             eval_time = eval_time,
                             interval = interval
  ) |>
    purrr::map(~ .x |>
                 dplyr::slice(1) |>
                 tidyr::unnest(col = .pred))

  table_pred_hazard <- extract_predictions(list_pred_hazard, ".pred_hazard")

  out <- c(
    list(list_pred_surv = list_pred_surv),
    list(list_pred_hazard = list_pred_hazard),
    list(table_pred_surv = table_pred_surv),
    list(table_pred_surv_lower = table_pred_surv_lower),
    list(table_pred_surv_upper = table_pred_surv_upper)
  )

  return(out)
}
