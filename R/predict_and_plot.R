#' Predict and plot \code{fit_models} on \code{data} at \code{eval_time}.
#'
#' This function generates survival and hazard predictions and plots for each
#' model in a \code{fit_models} object. Optionally, interactive \code{plotly}
#' outputs can be added for each plot.
#'
#' @param fit_models An object returned from fit_models.
#' @param eval_time (Optional) A vector of evaluation time points for generating
#'   predictions. Default is \code{NULL}, which if left as NULL, generates a
#'   sequence from 0 to 5 times the maximum observed time.
#' @param data The survival data used to estimate the models.
#' @param interval A character string specifying the type of interval to
#'   predict. Options are "none" or "confidence". Default is "none".
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
#' predict_and_plot(output_test, data = easysurv::easy_bc)
#'
predict_and_plot <- function(fit_models,
                             eval_time = NULL,
                             data,
                             interval = "none",
                             km_include = TRUE,
                             subtitle_include = TRUE,
                             add_plotly = FALSE) {
  # Create visible binding for R CMD check
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
    max_time <- max(data[[fit_models$info$time]], na.rm = TRUE)
    eval_time <- seq(0,
      ceiling(max_time * 5),
      length.out = 100
    )
  }

  ## Prepare KM data ----
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

  ## Prepare predictions ----
  predictions <- list()
  plots <- list()
  profiles <- NULL

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


  # Create the profile data based on covariates
  if (is.null(fit_models$info$covariates)) {
    used_profile <- data |> dplyr::slice(1)
  } else {
    used_profile <- create_newdata(
      data |>
        dplyr::select(dplyr::all_of(fit_models$info$covariates))
    )
    profiles <- list(profiles = used_profile)
  }

  # Set the loop labels based on the approach
  loop_labels <- if (inherits(fit_models, "pred_none")) {
    "All"
  } else {
    fit_models$info$predict_list
  }

  for (tx in seq_along(loop_labels)) {
    model_index <- if (inherits(fit_models, "pred_none") ||
      inherits(fit_models, "pred_covariate")) {
      1
    } else {
      tx
    }

    subtitle <- if (subtitle_include) {
      loop_labels[tx]
    } else {
      NULL
    }

    if (inherits(fit_models, "pred_covariate")) {
      # This relies on it being a factor variable.
      filtered_profile <- used_profile |>
        dplyr::filter(!!as.symbol(fit_models$info$predict_by) ==
          fit_models$info$predict_list[tx])
    } else {
      filtered_profile <- used_profile
    }

    predictions[[tx]] <- tidy_predict_surv(
      fit_models = fit_models,
      tx_index = tx,
      model_index = model_index,
      new_data = filtered_profile,
      eval_time = eval_time,
      interval = interval,
      special_profiles = !is.null(profiles)
    )

    filtered_km_df <- km_df |>
      dplyr::filter(group == tx)

    if (any(sapply(predictions[[tx]]$table_pred_surv, is.list))) {
      # there are multiple profiles
      plots[[tx]] <- list(
        surv_plots = lapply(
          predictions[[tx]]$table_pred_surv,
          plot_surv,
          km_data = filtered_km_df,
          km_include = km_include,
          subtitle = subtitle,
          legend_label = legend_label
        ),
        hazard_plots = lapply(
          predictions[[tx]]$table_pred_hazard,
          plot_hazards,
          obs_data = predictions[[tx]]$table_bshazard,
          subtitle = subtitle,
          legend_label = legend_label
        )
      )
    } else {
      plots[[tx]] <- list(
        surv_plots = plot_surv(
          pred_data = predictions[[tx]]$table_pred_surv,
          km_data = filtered_km_df,
          km_include = km_include,
          subtitle = subtitle,
          legend_label = legend_label
        ),
        hazard_plots = plot_hazards(
          pred_data = predictions[[tx]]$table_pred_hazard,
          obs_data = predictions[[tx]]$table_bshazard,
          subtitle = subtitle,
          legend_label = legend_label
        )
      )
    }
  }

  # Set names for predictions and plots
  names(predictions) <- names(plots) <- loop_labels

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
#' @importFrom cli cli_h1 cli_text cli_ul cli_li cli_end cli_alert_info
#' @noRd
print.pred_plot <- function(x, ...) {
  cli::cli_alert_info(paste0(
    "Survival and hazard predictions are stored in the prediction list. ",
    "Plots have been printed."
  ))

  # Suppress warnings, mainly to do with hazard plots for Gompertz models.
  for (tx in seq_along(x$plots)) {
    # Print surv_plots together
    suppressWarnings(print(x$plots[[tx]]$surv_plots))
  }

  for (tx in seq_along(x$plots)) {
    # Print hazard_plots together
    suppressWarnings(print(x$plots[[tx]]$hazard_plots))
  }

  invisible(x)
}
