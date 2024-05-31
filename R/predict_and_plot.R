#' Predict and plot \code{fit_models} on \code{data} at \code{eval_time}.
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
#'
#' @export
#'
#' @importFrom dplyr all_of filter select slice
#' @importFrom cli cli_abort
predict_and_plot <- function(fit_models,
                             eval_time = NULL,
                             data,
                             interval = "none",
                             km_include = TRUE) {

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
      group_vec <- mapply(rep,
                          seq_along(names(km_survfit[["strata"]])),
                          km_survfit[["strata"]]) |>
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

  predictions <- list()
  plots <- list()

  profiles <- NULL

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

    if (inherits(fit_models, "pred_covariate")) {
      filtered_profile <- used_profile |>
        dplyr::filter(!!as.symbol(fit_models$info$predict_by) ==
          fit_models$info$predict_list[tx])
    } else {
      filtered_profile <- used_profile
    }

    predictions[[tx]] <- tidy_predict_surv(
      models = fit_models$models[[model_index]],
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
        fit_plots = lapply(
          predictions[[tx]]$table_pred_surv,
          plot_fits,
          filtered_km_df,
          km_include
        )
      )
    } else {
      plots[[tx]] <- list(
        fit_plots = plot_fits(predictions[[tx]]$table_pred_surv, filtered_km_df, km_include)
      )
    }
  }

  # Set names for predictions and plots
  names(predictions) <- names(plots) <- loop_labels

  out <- list(
    profiles = profiles$profiles, predictions = predictions, plots = plots
  )

  class(out) <- c(class(out), "pred_plot")

  out
}


#' Print methods for \code{pred_plot}
#' @param x An object of class \code{pred_plot}
#' @param ... Additional arguments
#' @export
#' @importFrom cli cli_h1 cli_text cli_ul cli_li cli_end cli_alert_info
print.pred_plot <- function(x, ...) {
  cli::cli_h1("Predictions Summary")

  cli::cli_alert_info("TODO: Keep adding more information here. Likely
                      to look for any instances of table_pred_surv and just print those.")

  invisible(x)
}
