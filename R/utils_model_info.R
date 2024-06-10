# Non-exported helper functions.

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
      if (!is.null(median)) median[[i]] <- dplyr::rename_with(
        median[[i]], ~ paste0("median_", .x)
        )
      if (!is.null(restricted_mean)) restricted_mean[[i]] <- dplyr::rename_with(
        restricted_mean[[i]], ~ paste0("rmst_", .x)
        )
      if (!is.null(mean)) mean[[i]] <- dplyr::rename_with(
        mean[[i]], ~ paste0("mean_", .x)
        )

      out[[i]] <- data.frame(distribution = distribution)
      if (!is.null(names(myseq)[i])) out[[i]] <- cbind(out[[i]],
                                                       strata = names(myseq)[i])

      if (!is.null(median)) out[[i]] <- cbind(out[[i]],
                                              median[[i]])
      if (!is.null(restricted_mean)) out[[i]] <- cbind(out[[i]],
                                                       restricted_mean[[i]])
      if (!is.null(mean)) out[[i]] <- cbind(out[[i]],
                                            mean[[i]])
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
          median_est = predict(mod$fit,
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
      median_est <- predict(mod$fit,
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
      get_vcov <- models[[i]]$fit$cov |>
        as.data.frame()

      # Make the column names consistent between models (v1, v2, v3, ...)
      colnames(get_vcov) <- paste0("v", seq_along(models[[i]]$fit$coefficients))

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
          get_vcov <- cbind(rbind(get_vcov, 0),0)
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

  # Start the tibble with distribution, parameter and covariate_marker columns.
  out <- dplyr::bind_rows(out) |>
    dplyr::relocate("distribution", "parameter", "covariate_marker") |>
    tibble::as_tibble()

  out
}

#' @importFrom bshazard bshazard
#' @importFrom dplyr slice
#' @importFrom purrr map
#' @importFrom rlang .data
#' @importFrom stats predict
#' @importFrom tibble as_tibble
#' @importFrom tidyr unnest
#' @noRd
tidy_predict_surv <- function(fit_models,
                              tx_index = 1,
                              model_index = 1,
                              new_data,
                              eval_time,
                              interval = "none",
                              special_profiles = FALSE) {

  models <- fit_models$models[[model_index]]

  if (is.null(fit_models$info$nested)) {
    bs_data <- fit_models$info$data[[1]]
  } else {
    bs_data <- fit_models$info$nested[["data"]][[tx_index]]
  }

  #  Calculate smoothed estimate of hazards based on B-splines (bshazard)
  hazard_formula <- stats::as.formula(
    paste0("survival::Surv(time = ",
           fit_models$info$time,
           ", event = ",
           fit_models$info$event,
           ") ~ 1"))

  table_bshazard <- with(
    bshazard::bshazard(hazard_formula,
                       data = bs_data,
                       verbose = FALSE
    ),
    data.frame(time, hazard, lower.ci, upper.ci)
  ) |>
    dplyr::rename(
      est = .data$hazard,
      lcl = .data$lower.ci,
      ucl = .data$upper.ci
    )

  # Start with NULLs to make dropping them easy with c()
  list_pred_surv <-
    list_pred_hazard <-
    table_pred_surv <-
    table_pred_surv_lower <-
    table_pred_surv_upper <-
    table_pred_hazard <- NULL

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

  profiles <- nrow(new_data)

  if (profiles == 1) {
    # make the predictions (survival)
    list_pred_surv <- lapply(models,
      stats::predict,
      new_data = new_data,
      type = "survival",
      eval_time = eval_time,
      interval = interval
    ) |>
      purrr::map(~ .x |>
        tidyr::unnest(col = .pred))

    # Extract to summary tables
    table_pred_surv <- extract_predictions(list_pred_surv, ".pred_survival")

    # Label the columns
    table_pred_surv <- label_table(table_pred_surv)

    if (interval == "confidence" && models[[1]]$spec$engine != "survival") {
      table_pred_surv_lower <- extract_predictions(
        list_pred_surv,
        ".pred_lower"
      )
      table_pred_surv_upper <- extract_predictions(
        list_pred_surv,
        ".pred_upper"
      )

      # Label the columns
      table_pred_surv_lower <- label_table(table_pred_surv_lower)
      table_pred_surv_upper <- label_table(table_pred_surv_upper)
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
        tidyr::unnest(col = .pred))

    table_pred_hazard <- extract_predictions(list_pred_hazard, ".pred_hazard")

    # Label columns
    table_pred_hazard <- label_table(table_pred_hazard)

  }

  if (profiles > 1) {
    for (i in seq_len(profiles)) {
      # make the predictions (survival)
      list_pred_surv[[i]] <- lapply(models,
        stats::predict,
        new_data = dplyr::slice(new_data, i),
        type = "survival",
        eval_time = eval_time,
        interval = interval
      ) |> purrr::map(~ .x |>
        tidyr::unnest(col = .pred))

      # Extract to summary tables
      table_pred_surv[[i]] <-
        extract_predictions(list_pred_surv[[i]], ".pred_survival")
      if (interval == "confidence" && models[[1]]$spec$engine != "survival") {
        table_pred_surv_lower[[i]] <- extract_predictions(
          list_pred_surv[[i]],
          ".pred_lower"
        )
        table_pred_surv_upper[[i]] <- extract_predictions(
          list_pred_surv[[i]],
          ".pred_upper"
        )
      }


      # make the predictions (hazard)
      list_pred_hazard[[i]] <- lapply(models,
        stats::predict,
        new_data = dplyr::slice(new_data, i),
        type = "hazard",
        eval_time = eval_time,
        interval = interval
      ) |> purrr::map(~ .x |>
        tidyr::unnest(col = .pred))

      table_pred_hazard[[i]] <- extract_predictions(
        list_pred_hazard[[i]],
        ".pred_hazard"
      )
    }

    names(list_pred_surv) <-
      names(list_pred_hazard) <-
      names(table_pred_surv) <-
      names(table_pred_hazard) <-
      new_data$profile

    if (interval == "confidence" && models[[1]]$spec$engine != "survival") {
      names(table_pred_surv_lower) <-
        names(table_pred_surv_upper) <-
        new_data$profile
    }
  }

  out <- c(
    if (special_profiles) list(profiles = new_data),
    list(list_pred_surv = list_pred_surv),
    list(table_pred_surv = table_pred_surv),
    list(table_pred_surv_lower = table_pred_surv_lower),
    list(table_pred_surv_upper = table_pred_surv_upper),
    list(list_pred_hazard = list_pred_hazard),
    list(table_pred_hazard = table_pred_hazard),
    list(table_bshazard = table_bshazard)
  )

  out
}

# Helper functions

label_table <- function(df) {

  # Human readable label
  dist_labels <- c(
    "exp" = "Exponential",
    "exponential" = "Exponential",
    "gamma" = "Gamma",
    "genf" = "Gen. F",
    "genf.orig" = "Gen. F (orig parametrisation)",
    "gengamma" = "Gen. Gamma",
    "gengamma.orig" = "Gen. Gamma (orig parametrisation)",
    "gom" = "Gompertz",
    "gompertz" = "Gompertz",
    "llogis" = "log-Logistic",
    "lnorm" = "log-Normal",
    "lognormal" = "log-Normal",
    "weibull" = "Weibull (AFT)",
    "weibullPH" = "Weibull (PH)",
    "extreme" = "Extreme",
    "gaussian" = "Gaussian",
    "loggaussian" = "Log-Gaussian",
    "logistic" = "Logistic",
    "lognormal" = "Log-Normal",
    "rayleigh" = "Rayleigh"
  )

  # Get the current column names
  current_names <- colnames(df)

  # Map current names to readable labels using the lookup table
  new_names <- unname(sapply(current_names,
                             function(x) {
                               ifelse(x %in% names(dist_labels),
                                      dist_labels[x],
                                      x)
                               }))


  # Set the new column names
  colnames(df) <- new_names

  df

}
