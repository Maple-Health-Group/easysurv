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
    approach <- "predict_by_none"
  } else {
    predict_list <- levels(droplevels(as.factor(data[[predict_by]])))
    approach <- if (predict_by %in% covariates) "predict_by_covariate" else "predict_by_non_covariate"
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

  if (approach == "predict_by_none" | approach == "predict_by_covariate") {
    if (engine == "flexsurvspline") {

      out <- process_spline_combinations(k, scale, fit_formula, data)

    } else {

      out <- process_distributions(dists, fit_formula, data, engine)
    }

    models <- out$models
    distributions <- out$distributions
    if (engine == "flexsurvcure") cure_fractions <- out$cure_fractions


    goodness_of_fit <- get_goodness_of_fit(models)
    fit_averages <- suppressWarnings(get_fit_averages_summary(models))
    parameters <- get_surv_parameters(models)

  }

  if (approach == "predict_by_non_covariate") {
    predict_list <- levels(droplevels(as.factor(data[[predict_by]])))
    nested <- data |> tidyr::nest(.by = predict_by)

    for (tx in seq_along(predict_list)) {
      data_subset <- nested[["data"]][[tx]]

      if (engine == "flexsurvspline") {

        out <- process_spline_combinations(k, scale, fit_formula, data_subset)

      } else {

        out <- process_distributions(dists, fit_formula, data_subset, engine)
      }

      models[[tx]] <- out$models
      distributions[[tx]] <- out$distributions
      if (engine == "flexsurvcure") cure_fractions[[tx]] <- out$cure_fractions

      goodness_of_fit[[tx]] <- get_goodness_of_fit(models[[tx]])
      fit_averages[[tx]] <- suppressWarnings(get_fit_averages_summary(models[[tx]]))
      parameters[[tx]] <- get_surv_parameters(models[[tx]])

    }

    names(models) <-
      names(distributions) <-
      names(goodness_of_fit) <-
      names(fit_averages) <-
      names(parameters) <-
      predict_list

    if (engine == "flexsurvcure") names(cure_fractions) <- predict_list

  }


  # Predict and plot ----

  if (include_ci) {
    interval <- "confidence"
  } else {
    interval <- "none"
  }

  if (approach == "predict_by_none") {

    if (is.null(covariates)) {
      used_profile <- data |> dplyr::slice(1)
    } else {
      used_profile <- create_newdata(data |> dplyr::select(dplyr::all_of(covariates)))
      profiles <- list(profiles = used_profile)
    }

    predictions <- tidy_predict_surv(
      models = models,
      new_data = used_profile,
      eval_time = eval_time,
      interval = interval,
      special_profiles = !is.null(profiles)
    )

    if (any(sapply(predictions$table_pred_surv, is.list))) {
      # there are multiple profiles
      plots <- list(lapply(predictions$table_pred_surv, plot_fits))
    } else {
      plots <- list(fit_plots = plot_fits(predictions$table_pred_surv))
    }

    plots <- c(profiles, plots)

  }

  if (approach == "predict_by_covariate") {

    used_profile <- create_newdata(data |> dplyr::select(dplyr::all_of(covariates)))
    profiles <- list(profiles = used_profile)

    for (tx in seq_along(predict_list)) {

      # filtering the profiles so that predictions are group-specific
      filtered_profile <- used_profile |> dplyr::filter(!!as.symbol(predict_by) == predict_list[tx])

      predictions[[tx]] <- tidy_predict_surv(
        models = models,
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

    names(predictions) <-
      names(plots) <-
      predict_list

    plots <- c(profiles, plots)

  }

  if (approach == "predict_by_non_covariate") {

    if (is.null(covariates)) {
      used_profile <- data |> dplyr::slice(1)
    } else {
      used_profile <- create_newdata(data |> dplyr::select(dplyr::all_of(covariates)))
      profiles <- list(profiles = used_profile)
    }

    for (tx in seq_along(predict_list)) {

      # Not filtering because the predictions should be agnostic to the grouping
      # in the underlying data.

      predictions[[tx]] <- tidy_predict_surv(
        models = models[[tx]],
        new_data = used_profile,
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

    names(predictions) <- names(plots) <- names(models)

    plots <- c(profiles, plots)

  }

  # Return ----
  out <- list(
    approach = approach,
    engine = engine,
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
  class_name <- paste0("easy_", engine)
  class(out) <- c(class(out), class_name)

  return(out)
}
