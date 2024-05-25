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
#' @param engine (Optional) The survival analysis engine to be used.
#' Options are "flexsurv", "flexsurvcure", "flexsurvspline", and "survival".
#' Default is "flexsurv".
#' @param k (Optional) A numeric vector specifying the number of knots for
#' spline-based models. Default is \code{c(1, 2, 3)} to test different numbers.
#' @param scale (Optional) A character vector specifying the scale parameter(s)
#' for spline-based models. Options are "hazard", "odds", and "normal".
#' Default is \code{"hazard"}.
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
                       engine = "flexsurv",
                       k = c(1, 2, 3),
                       scale = c("hazard")) {

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

  info <- list(
    engine = engine,
    approach = approach,
    formula = deparse(fit_formula),
    time = time,
    event = event,
    covariates = covariates,
    predict_by = predict_by,
    predict_list = predict_list,
    distributions = distributions
  )

  # Return ----
  out <- list(
    info = info,
    models = models,
    goodness_of_fit = goodness_of_fit,
    fit_averages = fit_averages,
    cure_fractions = cure_fractions,
    parameters = parameters
  )

  # remove NULL
  out <- out |> purrr::discard(is.null)

  # Assign a class
  #class_name <- paste0("easy_", engine)
  class_fit_models <- "fit_models"
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
print.fit_models <- function(x, ...) {

  cli::cli_h1("Fit Models Summary")
  cli::cli_text("{.strong Engine:} {.field {x$info$engine}}.")
  cli::cli_text("{.strong Approach:} {.field {x$info$approach}}.")


  cli::cli_ul()
  if (inherits(x, "pred_none")) {
    cli::cli_li("The {.field predict_by} argument was not specified.")
    cli::cli_li("Therefore, models were fit on the full dataset.")
  }

  if (inherits(x, "pred_covariate")) {
    cli::cli_li("The {.field predict_by} argument was set to {.val {x$info$predict_by}},
                which was also a covariate.")
    cli::cli_li("Therefore, models were fit on the full dataset.")
    cli::cli_li("This is sometimes referred to as {.val joint fits}.")
  }

  if (inherits(x, "pred_other")) {
    cli::cli_li("The {.field predict_by} argument was set to {.val {x$info$predict_by}},
                which was not a covariate.")
    cli::cli_li("Therefore, models were fit for each level of {.val {x$info$predict_by}}.")
    cli::cli_li("This is sometimes referred to as {.val separate fits}.")
  }
  cli::cli_end()

  cli::cli_alert_info("TODO: Keep adding more information here.")

  invisible(x)
}
