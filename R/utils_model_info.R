#' Extract Schoenfeld Residuals
#'
#' This function extracts Schoenfeld residuals from a fitted `cox.zph` object
#' and formats them into a tidy data frame.
#'
#' @param fit_zph An object of class `cox.zph` produced by the `cox.zph`
#'   function, representing the Schoenfeld residuals of a Cox proportional
#'   hazards model.
#'
#' @return A tibble with the Schoenfeld residuals in long format, containing the
#' columns:
#' \item{time}{The time variable from the Cox model.}
#' \item{transform}{The transformation applied to the time variable.}
#' \item{variable}{The variable names from the Cox model for which residuals
#' are calculated.}
#' \item{residual}{The Schoenfeld residuals for each variable at each time
#' point.}
#'
#' @export
#'
#' @importFrom tibble as_tibble
#' @importFrom tidyr pivot_longer
#'
#' @examples
#'
#' library(survival)
#' test_fit <- survival::coxph(survival::Surv(time, status) ~ sex, data = lung)
#' test_fit_zph <- survival::cox.zph(test_fit)
#' get_schoenfeld(test_fit_zph)
get_schoenfeld <- function(fit_zph) {
  # Create visible binding for R CMD check.
  time <- NULL

  out <- tibble::as_tibble(fit_zph$y) |>
    cbind(
      time      = fit_zph$x,
      transform = fit_zph$transform
    ) |>
    tidyr::pivot_longer(c(-time, -transform),
      names_to = "variable",
      values_to = "residual"
    )

  out
}


# Non-exported helper functions ----

#' @importFrom dplyr everything select mutate row_number
#' @noRd
create_newdata <- function(data) {
  # Create visible binding for R CMD check.
  profile <- NULL

  # Identify factor and numeric columns
  factor_columns <- names(data)[sapply(data, is.factor)]
  numeric_columns <- names(data)[sapply(data, is.numeric)]

  # Calculate means for numeric columns
  numeric_means <- colMeans(data[, numeric_columns, drop = FALSE], na.rm = TRUE)

  # If there are factor columns, create all combinations of factor levels
  if (length(factor_columns) > 0) {
    factor_levels_combinations <- expand.grid(
      lapply(data[, factor_columns, drop = FALSE], levels)
    )

    # Add mean values for numeric columns to each row
    for (col in numeric_columns) {
      factor_levels_combinations[[col]] <- numeric_means[col]
    }

    # Ensure the order of columns matches the original data
    newdata <- factor_levels_combinations[colnames(data)]
  } else {
    # If there are no factor columns, create a data frame with mean values
    # for numeric columns
    newdata <- as.data.frame(
      matrix(numeric_means, ncol = length(numeric_columns))
    )
    colnames(newdata) <- numeric_columns
  }

  newdata <- newdata |>
    dplyr::mutate(profile = paste0("profile", dplyr::row_number())) |>
    dplyr::select(profile, dplyr::everything())

  newdata
}

#' @importFrom stats pnorm
#' @noRd
get_cure_fractions <- function(mod) {
  # Define the link function
  link_function <- mod$fit$link

  # Get theta
  theta <- mod$fit$res.t[1]

  # Calculate cure fraction based on the link function
  cure_fraction <- switch(link_function,
    logistic = exp(theta) / (1 + exp(theta)),
    loglog = exp(-exp(theta)),
    probit = stats::pnorm(theta),
    identity = max(0, min(1, theta)),
    stop("Unknown link function")
  )

  cure_fraction
}

#' @importFrom cli cli_abort
#' @importFrom data.table rbindlist
#' @importFrom dplyr rename_with
#' @importFrom stats predict
#' @noRd
get_fit_averages <- function(mod,
                             get_median = TRUE,
                             get_rmst = FALSE,
                             get_mean = FALSE) {
  if (!get_median && !get_rmst && !get_mean) {
    cli::cli_abort(c(
      paste0(
        "You need to include at least one average ",
        "(median, rmst, or mean) in the ",
        "get_fit_averages function"
      ),
      "x" = paste0(
        "You've provided {.field get_median} = {.var FALSE}, ",
        "{.field get_rmst} = {.var FALSE}, ",
        "and {.field get_mean} = {.var FALSE}."
      )
    ))
  }

  out <- list()
  median_est <- list()
  engine <- mod$spec$engine
  filtered_terms <- character(0)

  distribution <- if (engine %in% c("flexsurv", "flexsurvcure")) {
    mod$fit$dlist$name
  } else if (engine == "flexsurvspline") {
    paste(mod$fit$k, "knot", mod$fit$scale)
  } else {
    mod$fit$dist
  }

  calculate_summary <- function(type) {
    tryCatch(summary(mod$fit, type = type), error = function(e) {
      message(sprintf(
        "warning: %s %s survival time not calculable.",
        distribution,
        type
      ))
      list(as.data.frame(list("est" = "-", "lcl" = "-", "ucl" = "-")))
    })
  }

  if (engine %in% c("flexsurv", "flexsurvcure", "flexsurvspline")) {
    median <- if (get_median) calculate_summary("median") else NULL
    restricted_mean <- if (get_rmst) calculate_summary("rmst") else NULL
    mean <- if (get_mean) calculate_summary("mean") else NULL

    myseq <- if (!is.null(median)) {
      median
    } else if (!is.null(restricted_mean)) {
      restricted_mean
    } else {
      mean
    }

    for (i in seq_along(myseq)) {
      if (!is.null(median)) {
        median[[i]] <- dplyr::rename_with(
          median[[i]], ~ paste0("median_", .x)
        )
      }
      if (!is.null(restricted_mean)) {
        restricted_mean[[i]] <- dplyr::rename_with(
          restricted_mean[[i]], ~ paste0("rmst_", .x)
        )
      }
      if (!is.null(mean)) {
        mean[[i]] <- dplyr::rename_with(
          mean[[i]], ~ paste0("mean_", .x)
        )
      }

      out[[i]] <- data.frame(distribution = distribution)
      if (!is.null(names(myseq)[i])) {
        out[[i]] <- cbind(out[[i]],
          strata = names(myseq)[i]
        )
      }

      if (!is.null(median)) {
        out[[i]] <- cbind(
          out[[i]],
          median[[i]]
        )
      }
      if (!is.null(restricted_mean)) {
        out[[i]] <- cbind(
          out[[i]],
          restricted_mean[[i]]
        )
      }
      if (!is.null(mean)) {
        out[[i]] <- cbind(
          out[[i]],
          mean[[i]]
        )
      }
    }

    names(out) <- names(myseq)
    out <- data.table::rbindlist(out)
  } else if (engine == "survival") {
    # Check for groups
    # this only works for factors.
    # if there are multiple factors, this will need to be updated.
    # but suspect that "flexsurv" style engines will be preferred.
    if (!is.null(mod$fit$xlevels)) {
      # this is for factor variables
      n_xlevels <- length(mod$fit$xlevels[[1]])

      # assume for now that we only have one factor variable.
      term_labels <- attr(mod$fit$terms, "term.labels")
      # Get rid of ones that say "as.factor"
      filtered_terms <- term_labels[!grepl("as\\.factor\\(", term_labels)]
      # Get rid of the one for the main factor
      filtered_terms <- setdiff(filtered_terms, mod$fit$xlevels)

      for (i in seq_len(n_xlevels)) {
        out[[i]] <- data.frame(distribution = distribution)

        # Create a single row data frame for prediction data
        new_data <- data.frame(column1 = mod$fit$xlevels[[1]][i])
        # Rename the column to match that in the original data
        colnames(new_data) <- names(mod$fit$xlevels)

        if (!identical(filtered_terms, character(0))) {
          new_data <- c(new_data, mod$fit$means[filtered_terms])
        }

        # Get the median (IGNORE "new_data" warnings if appear, it does
        # in fact need to remain as "newdata".)
        median_est[[i]] <- data.frame(
          strata = mod$fit$xlevels[[1]][i],
          median_est = stats::predict(mod$fit,
            newdata = new_data,
            type = "quantile",
            p = c(0.5),
          )
        )

        out[[i]] <- cbind(out[[i]], median_est[[i]])
      }

      out <- data.table::rbindlist(out)
    } else {
      out <- data.frame(distribution = distribution)

      # just so it has a variable.
      new_data <- data.frame(testinggg = 123)

      # add relevant covariates if needed
      if (!identical(filtered_terms, character(0))) {
        new_data <- c(new_data, mod$fit$means[filtered_terms])
      }

      # Get the median (newdata does not matter)
      median_est <- stats::predict(mod$fit,
        newdata = new_data,
        type = "quantile",
        p = c(0.5)
      )

      out <- cbind(out, median_est)
    }
  }

  out
}

#' @importFrom data.table rbindlist
#' @importFrom tibble as_tibble
#' @noRd
get_fit_averages_summary <- function(models,
                                     get_median = TRUE,
                                     get_rmst = FALSE,
                                     get_mean = FALSE) {
  out <- lapply(models,
    get_fit_averages,
    get_median = get_median,
    get_rmst = get_rmst,
    get_mean = get_mean
  )

  out <- data.table::rbindlist(out) |> tibble::as_tibble()

  out
}

#' @importFrom stats AIC BIC
#' @importFrom tibble tibble
#' @noRd
get_goodness_of_fit <- function(mod) {
  # Get AIC and BIC values using stats:: because engine=survival
  # doesn't record these
  aic_values <- sapply(mod, function(x) stats::AIC(x$fit))
  bic_values <- sapply(mod, function(x) stats::BIC(x$fit))

  aic_ranks <- rank(aic_values)
  bic_ranks <- rank(bic_values)

  out <- tibble::tibble(
    "dist" = names(mod),
    "aic" = aic_values,
    "bic" = bic_values,
    "aic_rank" = aic_ranks,
    "bic_rank" = bic_ranks
  )

  out
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

    if (engine %in% c("flexsurv", "flexsurvcure", "flexsurvspline")) {
      # Get parameters from res.t
      get_parameters <- models[[i]]$fit$res.t |>
        as.data.frame() |>
        tibble::rownames_to_column(var = "parameter")

      # Get vcov matrix
      get_vcov <- models[[i]]$fit$cov

      # Make the column names consistent between models (v1, v2, v3, ...)
      # vcov can be logical=NA if model has struggled to fit but still returned
      if (!is.logical(get_vcov)) {
        get_vcov <- as.data.frame(get_vcov)
        colnames(get_vcov) <- paste0(
          "v",
          seq_along(models[[i]]$fit$coefficients)
        )
      }

      # Combine all results
      combined_results <- cbind(distribution, get_parameters, get_vcov)

      # Track location for covariate parameter
      combined_results$covariate_marker <- combined_results$parameter
      if (!is.null(models[[i]]$fit$covpars)) {
        combined_results$covariate_marker[models[[i]]$fit$covpars] <-
          models[[i]]$fit$dlist$location
      }
    } else if (engine == "survival") {
      # With the survival package, it's a bit tricky.

      # Get initial parameters from coefficients
      get_parameters <- models[[i]]$fit$coefficients |>
        as.data.frame() |>
        tibble::rownames_to_column(var = "parameter")
      colnames(get_parameters) <- c("parameter", "est")

      # If there's an additional parameter not included in coefficients, add it
      icoef_tail <- utils::tail(names(models[[i]]$fit$icoef), 1)
      coeff_tail <- utils::tail(names(models[[i]]$fit$coefficients), 1)
      if (icoef_tail != coeff_tail) {
        get_additional_parameters <- utils::tail(models[[i]]$fit$icoef, 1) |>
          as.data.frame() |>
          tibble::rownames_to_column(var = "parameter")
        colnames(get_additional_parameters) <- c("parameter", "est")

        get_parameters <- rbind(get_parameters, get_additional_parameters)

        if (get_parameters[length(get_parameters[, 1]), 2] == 0) {
          # Drop the last row if it's zero (e.g. for exponential log(scale))
          get_parameters <- get_parameters[-length(get_parameters[, 1]), ]
        }
      }

      # Get vcov matrix
      get_vcov <- models[[i]]$fit$var |>
        as.data.frame()

      if (models[[i]]$fit$dist == "rayleigh") {
        # The survival package does not include the Log(scale) parameter in the
        # vcov matrix for the Rayleigh distribution.

        # As this is an edge case, this is handled for now by adding 0s to the
        # vcov matrix.
        if (!identical(models[[i]]$fit$coefficients, models[[i]]$fit$icoef)) {
          get_vcov <- cbind(rbind(get_vcov, 0), 0)
          colnames(get_vcov) <- rownames(get_vcov) <- get_parameters$parameter
        }
      }

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

  # All failed models
  if (length(models) == 0) {
    return(out)
  }

  # Start the tibble with distribution, parameter and covariate_marker columns.
  out <- dplyr::bind_rows(out) |>
    dplyr::relocate("distribution", "parameter", "covariate_marker") |>
    tibble::as_tibble()

  out
}
