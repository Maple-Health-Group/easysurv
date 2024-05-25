#' Fit Survival Models
#'
#' Fits survival models to the provided data using the specified engine and
#' returns various outputs including model parameters, predictions, and plots.
#'
#' @param data A data frame containing the survival data.
#' @param time The name of the column in \code{data} containing the
#' time-to-event information.
#' @param event The name of the column in \code{data} indicating whether the
#' event of interest occurred.
#' @param predict_by (Optional) The name of the column in \code{data} defining the
#' prediction variable.
#' @param covariates (Optional) A character vector specifying the names of
#' covariates to be included in the model.
#' @param dists (Optional) A character vector specifying the distribution(s) to
#' be fitted.
#'
#' For flexsurv-based engines, options are "exp", "gamma", "genf", "genf.orig",
#' "gengamma", "gengamma.orig", "gompertz", "llogis", "lnorm", "lognormal",
#' "weibull", and "weibullPH".
#'
#' For the survival engine, options are "exponential", "extreme", "gaussian",
#' "loggaussian", "logistic", "lognormal", "rayleigh", "weibull".
#'
#' Default is \code{c("exp", "gamma", "gengamma", "gompertz",
#' "llogis", "lnorm", "weibull")} which applies to flexsurv-related engines.
#'
#'
#' @param eval_time (Optional) A vector of evaluation time points for generating
#' predictions. Default is \code{NULL}, which if left as NULL, generates a
#' sequence from 0 to 2.5 times the maximum observed time.
#' @param engine (Optional) The survival analysis engine to be used.
#' Options are "flexsurv", "flexsurvcure", "flexsurvspline", and "survival".
#' Default is "flexsurv".
#' @param k (Optional) A numeric vector specifying the number of knots for
#' spline-based models. Default is \code{c(1, 2, 3)} to test different numbers.
#' @param scale (Optional) A character vector specifying the scale parameter(s)
#' for spline-based models. Options are "hazard", "odds", and "normal".
#' Default is \code{"hazard"}.
#' @param include_ci (Optional) Logical indicating whether to include confidence
#'  intervals in the predictions. Default is \code{FALSE}.
#'
#' @return A list containing various outputs including model distributions,
#' parameters, predictions, plots, and summary statistics.
#' @export
#'
#' @importFrom survival survfit
#' @importFrom purrr discard
#' @importFrom stats as.formula
#' @importFrom tidyr nest
#'
#' @examples
#' \dontrun{
#'
#' output_separate <- fit_models(
#'   data = easysurv::easy_bc,
#'   time = "recyrs",
#'   event = "censrec",
#'   predict_by = "group",
#'   covariates = c("age", "treatment"))
#'
#' }
fit_models <- function(data,
                       time,
                       event,
                       predict_by = NULL,
                       covariates = NULL,
                       dists = c("exp",
                                 "gamma",
                                 "gengamma",
                                 "gompertz",
                                 "llogis",
                                 "lnorm",
                                 "weibull"),
                       eval_time = NULL,
                       engine = "flexsurv",
                       k = c(1, 2, 3),
                       scale = c("hazard"),
                       include_ci = FALSE) {

  # Create key objects ----
  distributions <- list()
  models <- list()
  goodness_of_fit <- list()
  fit_averages <- list()
  parameters <- list()
  predictions <- list()
  plots <- list()

  # Create NULL objects ----
  cure_fractions <- NULL
  profiles <- NULL

  # Validate argument inputs ----

  ## Check data ----
  # Is it a data frame?
  if (!is.data.frame(data)) {
    stop(
      paste0(
        "`data` does not have class `data.frame`."
      ),
      call. = FALSE
    )
  }

  # Are the required columns present?
  # note if predict_by is NULL, it is dropped.
  required_cols <- c(time, event, predict_by)

  if (!all(required_cols %in% names(data))) {
    stop(
      paste0(
        "The following columns are missing from `data`: ",
        paste0(required_cols[!required_cols %in% names(data)], collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # ...

  ## Check time ----
  # ...

  ## Check event ----
  # ...

  ## Check eval_time ----
  # If eval_time is missing, create a sequence from 0 to 2.5 times the maximum time
  if (is.null(eval_time)) {
    max_time <- max(data[[time]], na.rm = TRUE)
    eval_time <- seq(0,
                     ceiling(max_time * 2.5),
                     length.out = 100)
  }

  ## Check covariate approach ----

  # Set approach
  if (is.null(predict_by)) {
    predict_list <- NULL
    approach <- "predict_by_none"
  } else {
    predict_list <- levels(droplevels(as.factor(data[[predict_by]])))
    approach <- if (predict_by %in% covariates) "predict_by_covariate" else "predict_by_other"
  }

  covariates_string <- if (!is.null(covariates)) paste(covariates, collapse = " + ") else 1

  ## Check engine ----
  match.arg(engine,
            c("flexsurv", "flexsurvcure", "flexsurvspline", "survival"),
            several.ok = FALSE)

  ## Check dists ----

  # Check that dists has legal values for engine
  # ...

  # Create formula ----

  fit_formula <- stats::as.formula(paste0(
    "survival::Surv(time = ",
    time,
    ", event = ",
    event,
    ") ~",
    covariates_string
  ))


  # Fit models ----

  if (approach %in% c("predict_by_none", "predict_by_covariate")) {
    data_sets <- list(data)
    fit_labels <- "All"
  } else {
    nested <- data |> tidyr::nest(.by = predict_by)
    data_sets <- nested[["data"]]
    fit_labels <- predict_list
  }

  for (i in seq_along(data_sets)) {
    data_subset <- data_sets[[i]]
    if (engine == "flexsurvspline") {
      out <- process_spline_combinations(k, scale, fit_formula, data_subset)
    } else {
      out <- process_distributions(dists, fit_formula, data_subset, engine)
    }

    models[[i]] <- out$models
    distributions[[i]] <- out$distributions
    if (engine == "flexsurvcure") cure_fractions[[i]] <- out$cure_fractions
    goodness_of_fit[[i]] <- get_goodness_of_fit(models[[i]])
    fit_averages[[i]] <- suppressWarnings(get_fit_averages_summary(models[[i]]))
    parameters[[i]] <- get_surv_parameters(models[[i]])
  }

  names(models) <-
    names(distributions) <-
    names(parameters) <-
    names(goodness_of_fit) <-
    names(fit_averages) <-
    fit_labels
  if (engine == "flexsurvcure") names(cure_fractions) <- predict_list


  # Predict and plot ----

  # Define a helper function to generate predictions and plots
  generate_predictions_and_plots <- function(models,
                                             eval_time,
                                             interval,
                                             covariates,
                                             predict_by,
                                             predict_list,
                                             fit_labels,
                                             data,
                                             approach) {
    predictions <- list()
    plots <- list()

    # Create the profile data based on covariates
    if (is.null(covariates)) {
      used_profile <- data |> dplyr::slice(1)
    } else {
      used_profile <- create_newdata(data |> dplyr::select(dplyr::all_of(covariates)))
      profiles <- list(profiles = used_profile)
    }

    # Set the loop labels based on the approach
    loop_labels <- if (approach == "predict_by_none") fit_labels else predict_list

    for (tx in seq_along(loop_labels)) {

      model_index <- if (approach == "predict_by_none" | approach == "predict_by_covariate") 1 else tx

      if (approach == "predict_by_covariate") {
        filtered_profile <- used_profile |> dplyr::filter(!!as.symbol(predict_by) == predict_list[tx])
      } else {
        filtered_profile <- used_profile
      }

      predictions[[tx]] <- tidy_predict_surv(
        models = models[[model_index]],
        new_data = filtered_profile,
        eval_time = eval_time,
        interval = interval,
        special_profiles = !is.null(profiles)
      )

      if (any(sapply(predictions[[tx]]$table_pred_surv, is.list))) {
        # there are multiple profiles
        plots[[tx]] <- list(fit_plots = lapply(predictions[[tx]]$table_pred_surv, plot_fits))
      } else {
        plots[[tx]] <- list(fit_plots = plot_fits(predictions[[tx]]$table_pred_surv))
      }
    }

    # Set names for predictions and plots
    names(predictions) <- names(plots) <- loop_labels

    # Combine profiles with plots
    plots <- c(profiles, plots)

    return(list(predictions = predictions, plots = plots))
  }

  if (include_ci) {
    interval <- "confidence"
  } else {
    interval <- "none"
  }

  predictions_and_plots <- generate_predictions_and_plots(models = models,
                                                          eval_time = eval_time,
                                                          interval = interval,
                                                          covariates = covariates,
                                                          predict_by = predict_by,
                                                          predict_list = predict_list,
                                                          fit_labels = fit_labels,
                                                          data = data,
                                                          approach = approach)
  predictions <- predictions_and_plots$predictions
  plots <- predictions_and_plots$plots

  # Return ----
  out <- list(
    engine = engine,
    approach = approach,
    formula = deparse(fit_formula),
    covariates = covariates,
    predict_by = predict_by,
    distributions = distributions,
    models = models,
    goodness_of_fit = goodness_of_fit,
    fit_averages = fit_averages,
    cure_fractions = cure_fractions,
    parameters = parameters,
    predictions = predictions,
    plots = plots
  )

  # remove NULL
  out <- out |> purrr::discard(is.null)

  # Assign a class
  #class_name <- paste0("easy_", engine)
  class_fit_models <- "easy_fit_models"
  class_approach <- switch(
    approach,
    predict_by_none = "pred_none",
    predict_by_covariate = "pred_covariate",
    predict_by_other = "pred_other"
  )
  class(out) <- c(class(out), class_fit_models, class_approach)

  return(out)
}

#' Print methods for \code{fit_models}
#' @param x An object of class \code{fit_models}
#' @param ... Additional arguments
#' @export
#' @importFrom cli cli_h1 cli_text cli_ul cli_li cli_end cli_alert_info
print.easy_fit_models <- function(x, ...) {

  cli::cli_h1("Fit Models Summary")
  cli::cli_text("{.strong Engine:} {.field {x$engine}}.")
  cli::cli_text("{.strong Approach:} {.field {x$approach}}.")


  cli::cli_ul()
  if (inherits(x, "pred_none")) {
    cli::cli_li("The {.field predict_by} argument was not specified.")
    cli::cli_li("Therefore, models were fit on the full dataset.")
  }

  if (inherits(x, "pred_covariate")) {
    cli::cli_li("The {.field predict_by} argument was set to {.val {x$predict_by}},
                which was also a covariate.")
    cli::cli_li("Therefore, models were fit on the full dataset.")
    cli::cli_li("This is sometimes referred to as {.val joint fits}.")
  }

  if (inherits(x, "pred_other")) {
    cli::cli_li("The {.field predict_by} argument was set to {.val {x$predict_by}},
                which was not a covariate.")
    cli::cli_li("Therefore, models were fit for each level of {.val {x$predict_by}}.")
    cli::cli_li("This is sometimes referred to as {.val separate fits}.")
  }
  cli::cli_end()

  cli::cli_alert_info("TODO: Keep adding more information here.")

  invisible(x)
}
