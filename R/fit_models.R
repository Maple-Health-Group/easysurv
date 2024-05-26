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
#' @importFrom dplyr select
#' @importFrom purrr discard
#' @importFrom stats as.formula
#' @importFrom survival survfit
#' @importFrom tibble rownames_to_column
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

  # Create formulae ----

  fit_formula <- stats::as.formula(paste0(
    "survival::Surv(time = ",
    time,
    ", event = ",
    event,
    ") ~",
    covariates_string
  ))

  km_formula <- stats::as.formula(paste0(
    "survival::Surv(time = ",
    time,
    ", event = ",
    event,
    ") ~",
    if (is.null(predict_by)) "1" else predict_by
  ))

  # Fit KM ----
  KM_fit <- do.call(survival::survfit,
                     args = list(
                       formula = km_formula,
                       conf.int = 0.95,
                       data = data,
                       type = "kaplan-meier"
                     )
  )

  # TODO: Suspect this will only work for flexsurv engines.
  KM_summary <- as.data.frame(summary(KM_fit)$table) |>
    tibble::rownames_to_column(var = "group") |>
    dplyr::select(-"n.max", -"n.start")


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
  if (engine == "flexsurvcure") names(cure_fractions) <- fit_labels

  info <- list(
    engine = engine,
    approach = approach,
    formula = deparse(fit_formula),
    time = time,
    event = event,
    covariates = covariates,
    predict_by = predict_by,
    predict_list = predict_list,
    distributions = distributions,
    KM_summary = KM_summary
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
#' @importFrom cli cli_h1 cli_h2 cli_h3 cli_text
#' @importFrom cli cli_ul cli_li cli_div cli_end
#' @importFrom cli cli_alert cli_alert_info cli_alert_warning cli_rule
#' @importFrom cli cat_line qty
#' @importFrom dplyr select filter pull
#' @importFrom tidyr pivot_wider
print.fit_models <- function(x, ...) {

  # Create visible binding for R CMD check.
  distribution <- strata <- dist <- AIC_rank <- NULL

  cli::cli_h1("Fit Models Summary")

  cli::cli_text("{.strong Engine:} {.field {x$info$engine}}.")

  # Approach
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

  # Distributions
  cli::cat_line()
  cli::cli_text("{.strong Distributions attempted:} {.val {x$info$distributions[[1]]$dists_attempted}}.")

  cli::cli_h2("Median survival estimates")

  if (length(x$info$distributions) == 1) {

    # There's only one set of distributions to look at.
    if (length(x$info$distributions[[1]]$dists_failed) > 0) {
      cli::cli_alert_warning("Some distributions failed to converge.")
      cli::cli_text("Failed distributions: {.val {x$info$distributions[[1]]$dists_failed}}")
    }

    if (inherits(x, "pred_covariate")) {

      median.est <- x$fit_averages[[1]] |>
        dplyr::select(distribution, strata, median.est) |>
        tidyr::pivot_wider(names_from = "strata", values_from = "median.est") |>
        dplyr::select(-distribution)

    } else {

      median.est <- x$fit_averages[[1]]$median.est

    }

    # Goodness of fits and fit averages
    combined_data <- x$goodness_of_fit[[1]] |>
      select(dist, AIC_rank) |>
      cbind(median.est)

    # say what dist had the AIC_rank == 1
    best_dist <- combined_data |>
      dplyr::filter(AIC_rank == 1) |>
      dplyr::pull(dist) |>
      unique()

    data_median <- x$info$KM_summary$median

    print(combined_data)
    cli::cat_line()

    divid <- cli::cli_div(theme = list(.val = list(digits = 3)))
    cli::cli_alert_info("For comparison, the KM median survival {cli::qty(length(x$info$predict_list))}time{?s} {?was/were} {.val {data_median}}.")
    cli::cli_end(divid)

    cli::cli_alert_info("The distribution with the best (lowest) AIC was {.val {best_dist}}.")

  } else {
    # There are multiple distribution sets to look at.
    for (i in seq_along(x$info$predict_list)) {
      cli::cli_h3("Group: {.val {x$info$predict_list[i]}}")

      if (length(x$info$distributions[[i]]$dists_failed) > 0) {
        cli::cli_alert_warning("Failed distributions for {.val {names(x$info$distributions)[i]}}: {.val {x$info$distributions[[i]]$dists_failed}}")
      }

      if (inherits(x, "pred_covariate")) {

        median.est <- x$fit_averages[[1]] |>
          dplyr::select(distribution, strata, median.est) |>
          tidyr::pivot_wider(names_from = "strata", values_from = "median.est") |>
          dplyr::select(-distribution)

      } else {

        median.est <- x$fit_averages[[i]]$median.est

      }

      # Goodness of fits and fit averages
      combined_data <- x$goodness_of_fit[[i]] |>
        select(dist, AIC_rank) |>
        cbind(median.est)

      # say what dist had the AIC_rank == 1
      best_dist <- combined_data |>
        dplyr::filter(AIC_rank == 1) |>
        dplyr::pull(dist) |>
        unique()

      data_median <- x$info$KM_summary$median[i]
      print(combined_data)
      cli::cat_line()
      divid <- cli::cli_div(theme = list(.val = list(digits = 3)))
      cli::cli_alert_info("For comparison, the KM median survival time was {.val {data_median}}.")
      cli::cli_end(divid)
      cli::cli_alert_info("The distribution with the best (lowest) AIC was {.val {best_dist}}.")
    }
  }

  if (x$info$engine == "flexsurvcure") {
    cli::cli_h2("Cure Fractions")
    for (i in seq_along(x$info$distributions)) {
      cli::cli_h3("Group: {.val {names(x$info$distributions)[i]}}")
      print(x$cure_fractions[[i]])
    }
  }

  cli::cli_rule()
  cli::cli_alert("For more information, run {.code View()} on the fit_models output.")

  invisible(x)
}
