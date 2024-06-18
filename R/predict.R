#' Predict method for \code{fit_models}
#' @param object An object of class \code{fit_models}
#' @param eval_time (Optional) A vector of evaluation time points for generating
#'   predictions. Default is \code{NULL}, which if left as NULL, generates a
#'   sequence from 0 to 5 times the maximum observed time.
#' @param type A character vector indicating the type of predictions to
#'   generate. Default is \code{c("survival", "hazard")}.
#' @param ... Additional arguments
#' @export
#' @importFrom dplyr all_of filter slice
predict.fit_models <- function(object,
                               eval_time = NULL,
                               type = c("survival", "hazard"),
                               ...) {
  predictions <- list()
  profiles <- NULL

  ## Check eval_time ----

  # If eval_time is missing, create a sequence from 0 to 5* the maximum time
  if (is.null(eval_time)) {
    max_time <- max(object$info$data[[object$info$time]], na.rm = TRUE)

    eval_time <- seq(0,
      ceiling(max_time * 5),
      length.out = 100
    )
  }

  ## Check type ----
  rlang::arg_match(type,
    c("survival", "hazard"),
    multiple = TRUE
  )

  # Check for extra arguments
  extra_args <- names(list(...))
  if ("new_data" %in% extra_args) {
    cli::cli_abort(c(paste0(
      "You've provided a {.field new_data} argument, which is not accepted",
      " by {.fn predict for fit_models}. This is because new_data is inferred",
      " automatically. You can use {.field new_data} with predict on",
      " individual models if desired."
    )))
  }

  # Create the profile data based on covariates
  if (is.null(object$info$covariates)) {
    used_profile <- object$info$data |> dplyr::slice(1)
  } else {
    used_profile <- create_newdata(
      object$info$data |>
        dplyr::select(dplyr::all_of(object$info$covariates))
    )
    profiles <- list(profiles = used_profile)
  }

  # Set the loop labels based on the approach
  loop_labels <- if (inherits(object, "pred_none")) {
    "All"
  } else {
    object$info$predict_list
  }

  for (tx in seq_along(loop_labels)) {
    model_index <- if (inherits(object, "pred_none") ||
      inherits(object, "pred_covariate")) {
      1
    } else {
      tx
    }

    if (inherits(object, "pred_covariate")) {
      # This relies on it being a factor variable.
      filtered_profile <- used_profile |>
        dplyr::filter(!!as.symbol(object$info$predict_by) ==
          object$info$predict_list[tx])
    } else {
      filtered_profile <- used_profile
    }

    predictions[[tx]] <- predict_helper(
      models = object$models[[model_index]],
      new_data = filtered_profile,
      eval_time = eval_time,
      type = type,
      special_profiles = !is.null(profiles)
    )

    predictions[[tx]] <- c(
      predictions[[tx]],
      list(bshazard = get_bshazard(
        object,
        tx_index = tx
      ))
    )
  }

  names(predictions) <- loop_labels

  predictions
}

#' Predict and Plot Fitted Models
#'
#' This function generates survival and hazard predictions and plots for each
#' model in a \code{fit_models} object. Optionally, interactive \code{plotly}
#' outputs can be added for each plot.
#'
#' @param fit_models An object returned from fit_models.
#' @param eval_time (Optional) A vector of evaluation time points for generating
#'   predictions. Default is \code{NULL}, which if left as NULL, generates a
#'   sequence from 0 to 5 times the maximum observed time.
#' @param km_include A logical indicating whether to include Kaplan-Meier
#'  estimates in the plot outputs. Default is \code{TRUE}.
#' @param subtitle_include A logical indicating whether to include the subtitle.
#'  Default is \code{TRUE}. The subtitle is the name of the group.
#' @param add_plotly A logical indicating whether to add interactive plotly
#'   outputs for each plot. Default is \code{FALSE}.
#'
#' @export
#'
#' @importFrom dplyr all_of filter select slice
#' @importFrom cli cli_abort
#'
#' @examplesIf interactive()
#'
#' output_test <- fit_models(
#'   data = easysurv::easy_bc,
#'   time = "recyrs",
#'   event = "censrec",
#'   predict_by = "group"
#' )
#'
#' predict_and_plot(output_test)
predict_and_plot <- function(fit_models,
                             eval_time = NULL,
                             km_include = TRUE,
                             subtitle_include = TRUE,
                             add_plotly = FALSE) {
  group <- NULL

  ## Check fit_models ----
  if (!inherits(fit_models, "fit_models")) {
    cli::cli_abort(c(
      paste0(
        "The {.var fit_models} argument must be an object returned from ",
        "{.fn fit_models}."
      ),
      "x" = "You've provided an object of class: {.cls {class(fit_models)}}"
    ))
  }

  ## Check eval_time ----
  # If eval_time is missing, create a sequence from 0 to 5* the maximum time
  if (is.null(eval_time)) {
    max_time <- max(fit_models$info$data[[fit_models$info$time]], na.rm = TRUE)
    eval_time <- seq(0,
      ceiling(max_time * 5),
      length.out = 100
    )
  }

  ## Prepare KM data ----

  km_df <- NULL
  filtered_km_df <- NULL

  if (km_include) {
    km_survfit <- fit_models[["info"]][["km"]]
    if (is.null(km_survfit[["strata"]])) {
      group_vec <- rep(1, length(km_survfit[["time"]]))
    } else {
      group_vec <- mapply(
        rep,
        seq_along(names(km_survfit[["strata"]])),
        km_survfit[["strata"]]
      ) |>
        unlist() |>
        unname()
    }

    km_df <- data.frame(
      time  = km_survfit$time,
      surv  = km_survfit$surv,
      upper = km_survfit$upper,
      lower = km_survfit$lower,
      group = group_vec
    )
  }



  # Labels ----
  # Create legend label for the plots
  if (fit_models$info$engine == "flexsurvcure") {
    legend_label <- "Cure Model"
  } else if (fit_models$info$engine == "flexsurvspline") {
    legend_label <- "Spline Model"
  } else if (fit_models$info$approach == "predict_by_covariate") {
    legend_label <- "Joint Model"
  } else {
    legend_label <- "Model"
  }

  # Set the loop labels based on the approach
  loop_labels <- if (inherits(fit_models, "pred_none")) {
    "All"
  } else {
    fit_models$info$predict_list
  }


  ## Prepare predictions ----
  predictions <- predict.fit_models(fit_models, eval_time = eval_time)

  ## Prepare plots ----
  plots <- list()

  for (tx in seq_along(predictions)) {
    subtitle <- if (subtitle_include) {
      loop_labels[tx]
    } else {
      NULL
    }

    if (km_include) {
      filtered_km_df <- km_df |>
        dplyr::filter(group == tx)
    }


    if (any(sapply(predictions[[tx]]$predicted_surv, is.list)) ||
      any(sapply(predictions[[tx]]$predicted_hazard, is.list))) {
      # Initialize the list for the current tx element
      plots[[tx]] <- list()

      # Check if predicted_surv exists before attempting to use it
      if (!is.null(predictions[[tx]]$predicted_surv)) {
        plots[[tx]]$surv_plots <- lapply(
          predictions[[tx]]$predicted_surv,
          plot_surv,
          km_data = filtered_km_df,
          km_include = km_include,
          subtitle = subtitle,
          legend_label = legend_label
        )
      }

      # Check if predicted_hazard exists before attempting to use it
      if (!is.null(predictions[[tx]]$predicted_hazard)) {
        plots[[tx]]$hazard_plots <- lapply(
          predictions[[tx]]$predicted_hazard,
          plot_hazards,
          obs_data = predictions[[tx]]$bshazard,
          subtitle = subtitle,
          legend_label = legend_label
        )
      }
    } else {
      # Initialize the list for the current tx element
      plots[[tx]] <- list()

      # Check if predicted_surv exists before attempting to use it
      if (!is.null(predictions[[tx]]$predicted_surv)) {
        plots[[tx]]$surv_plots <- plot_surv(
          pred_data = predictions[[tx]]$predicted_surv,
          km_data = filtered_km_df,
          km_include = km_include,
          subtitle = subtitle,
          legend_label = legend_label
        )
      }

      # Check if predicted_hazard exists before attempting to use it
      if (!is.null(predictions[[tx]]$predicted_hazard)) {
        plots[[tx]]$hazard_plots <- plot_hazards(
          pred_data = predictions[[tx]]$predicted_hazard,
          obs_data = predictions[[tx]]$bshazard,
          subtitle = subtitle,
          legend_label = legend_label
        )
      }
    }
  }

  names(plots) <- loop_labels

  profiles <- NULL

  if (!is.null(fit_models$info$covariates)) {
    used_profile <- create_newdata(
      fit_models$info$data |>
        dplyr::select(dplyr::all_of(fit_models$info$covariates))
    )
    profiles <- list(profiles = used_profile)
  }

  out <- list(
    profiles = profiles$profiles, predictions = predictions, plots = plots
  )

  if (add_plotly) {
    plotly <- plots

    for (i in seq_along(plots)) {
      plotly[[i]]$surv_plots <- plotly_surv(plots[[i]]$surv_plots)

      plotly[[i]]$hazard_plots <- plotly_hazards(plots[[i]]$hazard_plots)
    }

    names(plotly) <- names(plots)

    out$plotly <- plotly
  }

  class(out) <- c(class(out), "pred_plot")

  out
}


#' Print methods for \code{pred_plot}
#' @param x An object of class \code{pred_plot}
#' @param ... Additional arguments
#' @export
#' @importFrom cli cli_alert_info
#' @noRd
print.pred_plot <- function(x, ...) {
  # Print messages at the beginning, since printing during was not respecting
  # the order of the code.
  if (!is.null(x$plots[[1]]$surv_plots)) {
    cli::cli_alert_info(paste0(
      "Survival plots have been printed."
    ))
  }

  if (!is.null(x$plots[[1]]$hazard_plots)) {
    cli::cli_alert_info(paste0(
      "Hazard plots have been printed."
    ))
  }

  # Suppress warnings, mainly to do with hazard plots for Gompertz models.
  for (tx in seq_along(x$plots)) {
    # Print surv_plots together
    if (!is.null(x$plots[[tx]]$surv_plots)) {
      suppressWarnings(print(x$plots[[tx]]$surv_plots))
    }
  }

  # Suppress warnings, mainly to do with hazard plots for Gompertz models.
  for (tx in seq_along(x$plots)) {
    # Print hazard_plots together
    if (!is.null(x$plots[[tx]]$hazard_plots)) {
      suppressWarnings(print(x$plots[[tx]]$hazard_plots))
    }
  }

  invisible(x)
}


predict_helper <- function(models, new_data, eval_time,
                           type = c("survival", "hazard"),
                           special_profiles = FALSE) {
  predicted_surv <- list()
  predicted_hazards <- list()
  n_profiles <- nrow(new_data)

  if (n_profiles == 1) {
    # make the predictions

    if ("survival" %in% type) {
      predicted_surv <- get_predict_table(
        models = models,
        new_data = new_data,
        eval_time = eval_time,
        type = "survival"
      )
    }

    if ("hazard" %in% type) {
      predicted_hazards <- get_predict_table(
        models = models,
        new_data = new_data,
        eval_time = eval_time,
        type = "hazard"
      )
    }
  }

  if (n_profiles > 1) {
    for (i in seq_len(n_profiles)) {
      # make the predictions

      if ("survival" %in% type) {
        predicted_surv[[i]] <- get_predict_table(
          models = models,
          new_data = dplyr::slice(new_data, i),
          eval_time = eval_time,
          type = "survival"
        )
      }

      if ("hazard" %in% type) {
        predicted_hazards[[i]] <- get_predict_table(
          models = models,
          new_data = dplyr::slice(new_data, i),
          eval_time = eval_time,
          type = "hazard"
        )
      }
    }

    if ("survival" %in% type) {
      names(predicted_surv) <- new_data$profile
    }

    if ("hazard" %in% type) {
      names(predicted_hazards) <- new_data$profile
    }
  }

  out <- c(
    if (special_profiles) list(profiles = new_data),
    if ("survival" %in% type) list(predicted_surv = predicted_surv),
    if ("hazard" %in% type) list(predicted_hazards = predicted_hazards)
  )

  out
}

#' @importFrom purrr map
#' @importFrom stats predict
#' @importFrom tibble as_tibble
#' @importFrom tidyr unnest
#' @noRd
get_predict_table <- function(models, new_data, eval_time, type) {
  predict_list <- lapply(models, stats::predict,
    new_data = new_data,
    type = type,
    eval_time = eval_time
  ) |>
    purrr::map(~ .x |> tidyr::unnest(col = .pred))

  col_name <- paste0(".pred_", type)

  # Extract to summary tables
  out <- Reduce(
    function(x, y) merge(x, y, by = ".eval_time", all = TRUE),
    lapply(names(predict_list), function(model) {
      df <- predict_list[[model]][, c(".eval_time", col_name)]
      colnames(df)[2] <- model
      return(df)
    })
  ) |> tibble::as_tibble()

  # Label the columns
  out <- label_table(out)

  out
}

#' @importFrom bshazard bshazard
#' @importFrom dplyr rename
#' @importFrom stats as.formula
#' @noRd
get_bshazard <- function(fit_models, tx_index = 1) {
  if (is.null(fit_models$info$nested)) {
    bs_data <- fit_models$info$data
  } else {
    bs_data <- fit_models$info$nested[["data"]][[tx_index]]
  }

  #  Calculate smoothed estimate of hazards based on B-splines (bshazard)
  hazard_formula <- stats::as.formula(
    paste0(
      "survival::Surv(time = ",
      fit_models$info$time,
      ", event = ",
      fit_models$info$event,
      ") ~ 1"
    )
  )

  table_bshazard <- with(
    bshazard::bshazard(hazard_formula,
      data = bs_data,
      verbose = FALSE
    ),
    data.frame(time, hazard, lower.ci, upper.ci)
  ) |>
    dplyr::rename(
      lcl = "lower.ci",
      ucl = "upper.ci"
    )

  table_bshazard
}


# Helper functions

#' Help Label Distributions
#' @noRd
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
  new_names <- unname(sapply(
    current_names,
    function(x) {
      ifelse(x %in% names(dist_labels),
        dist_labels[x],
        x
      )
    }
  ))

  # Set the new column names
  colnames(df) <- new_names

  df
}
