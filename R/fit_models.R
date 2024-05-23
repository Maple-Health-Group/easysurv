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
#' @param group (Optional) The name of the column in \code{data} defining the
#' grouping variable.
#' @param group_as_covariate (Optional) Logical indicating whether the grouping
#' variable should be treated as a covariate.
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
#' @importFrom purrr discard
#' @importFrom stats as.formula
#' @importFrom survminer ggsurvplot surv_fit
#' @importFrom tidyr nest
#'
#' @examples
#' \dontrun{
#'
#' output_separate <- fit_models(
#'   data = easysurv::easy_bc,
#'   time = "recyrs",
#'   event = "censrec",
#'   group = "group",
#'   group_as_covariate = FALSE)
#'
#' }
fit_models <- function(data,
                       time,
                       event,
                       group = NULL,
                       group_as_covariate = FALSE,
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
  parameters <- list()
  predictions <- list()
  plots <- list()
  summary <- list()

  # Create NULL objects ----
  # KM is here for now in case we want to add it to fit plots...
  KM <- NULL
  KM_plot <- NULL
  cure_fractions <- NULL

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
  # note if group is NULL, it is dropped.
  required_cols <- c(time, event, group)

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
  if (is.null(group)) {
    approach <- "no_groups"
    fit_covariate <- 1
    KM_covariate <- 1
  } else {
    group_list <- levels(droplevels(as.factor(data[[group]])))
    KM_covariate <- group

    if (group_as_covariate) {
      approach <- "joint_fits"
      fit_covariate <- group
    } else {
      approach <- "separate_fits"
      fit_covariate <- 1
    }
  }


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
    fit_covariate
  ))

  KM_formula <- stats::as.formula(paste0(
    "survival::Surv(time = ",
    time,
    ", event = ",
    event,
    ") ~",
    KM_covariate
  ))

  # Create KMs
  KM <- survminer::surv_fit(
    formula = KM_formula,
    conf.int = 0.95,
    data = data,
    type = "kaplan-meier"
  )

  KM_plot <- survminer::ggsurvplot(KM, data = data, ggtheme = theme_bw())


  # Fit models ----

  if (approach == "no_groups" | approach == "joint_fits") {
    if (engine == "flexsurvspline") {

      out <- process_spline_combinations(k, scale, fit_formula, data)

    } else {

      out <- process_distributions(dists, fit_formula, data, engine)
    }

    models <- out$models
    distributions <- out$distributions
    if (engine == "flexsurvcure") cure_fractions <- out$cure_fractions

    parameters <- get_surv_parameters(models)

    summary <- list(fit_averages = get_fit_averages_summary(models),
                    goodness_of_fit = get_goodness_of_fit(models))

  }

  if (approach == "separate_fits") {
    group_list <- levels(droplevels(as.factor(data[[group]])))
    nested <- data |> tidyr::nest(.by = group)

    for (tx in seq_along(group_list)) {
      data_subset <- nested[["data"]][[tx]]

      if (engine == "flexsurvspline") {

        out <- process_spline_combinations(k, scale, fit_formula, data_subset)

      } else {

        out <- process_distributions(dists, fit_formula, data_subset, engine)
      }

      models[[tx]] <- out$models
      distributions[[tx]] <- out$distributions
      if (engine == "flexsurvcure") cure_fractions[[tx]] <- out$cure_fractions

      parameters[[tx]] <- get_surv_parameters(models[[tx]])

      summary[[tx]] <- list(fit_averages = get_fit_averages_summary(models[[tx]]),
                            goodness_of_fit = get_goodness_of_fit(models[[tx]]))

    }

    names(models) <-
      names(distributions) <-
      names(parameters) <-
      names(summary) <-
      group_list

    if (engine == "flexsurvcure") names(cure_fractions) <- group_list

  }


  # Predict and plot ----

  if (include_ci) {
    interval <- "confidence"
  } else {
    interval <- "none"
  }

  if (approach == "no_groups") {
    predictions <- tidy_predict_surv(
      models = models,
      new_data = data,
      eval_time = eval_time,
      interval = interval
    )

    plots <- plot_fits(predictions$table_pred_surv)
  }

  if (approach == "joint_fits") {
    for (tx in seq_along(group_list)) {
      predictions[[tx]] <- tidy_predict_surv(
        models = models,
        new_data = data.frame(group = group_list[tx]),
        eval_time = eval_time,
        interval = interval
      )

      plots[[tx]] <- plot_fits(predictions[[tx]]$table_pred_surv)
    }

    names(predictions) <- names(plots) <- group_list
  }

  if (approach == "separate_fits") {
    for (tx in seq_along(group_list)) {
      predictions[[tx]] <- tidy_predict_surv(
        models = models[[tx]],
        new_data = data.frame(group = group_list[tx]),
        eval_time = eval_time,
        interval = interval
      )

      plots[[tx]] <- plot_fits(predictions[[tx]]$table_pred_surv)
    }

    names(predictions) <- names(plots) <- names(models)
  }

  # Create summary ----




  # Return ----
  out <- list(
    approach = approach,
    engine = engine,
    distributions = distributions,
    models = models,
    cure_fractions = cure_fractions,
    parameters = parameters,
    predictions = predictions,
    plots = plots,
    summary = summary,
    KM = KM,
    KM_plot = KM_plot
  )

  # remove NULL
  out <- out |> purrr::discard(is.null)

  return(out)
}
