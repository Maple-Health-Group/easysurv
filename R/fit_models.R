###########
# This file WAS CREATED for the new easysurv release.
###########

#' @importFrom data.table rbindlist
#' @importFrom parsnip survival_reg set_engine
#' @importFrom purrr map discard keep pmap pmap_chr set_names
#' @importFrom stats as.formula
#' @importFrom survminer ggsurvplot surv_fit
#' @importFrom tibble as_tibble
#' @importFrom tidyr expand_grid
#'
#' @export
#'
fit_models <- function(data,
                       time,
                       event,
                       group = NULL,
                       group_as_covariate = FALSE,
                       dists = c("exp", "gamma", "gengamma", "gompertz", "llogis", "lnorm", "weibull"),
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
  KM <- NULL
  KM_plot <- NULL

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
    eval_time <- seq(
      from = 0, to = ceiling(max(data[[time]]) * 2.5),
      length.out = 100
    )
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
  }

  if (!is.null(group) & group_as_covariate) {
    approach <- "joint_fits"
    fit_covariate <- group
  }

  if (!is.null(group) & !group_as_covariate) {
    approach <- "separate_fits"
    fit_covariate <- 1
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
      combinations <- tidyr::expand_grid(k, scale)

      models <- purrr::pmap(combinations, function(k, scale) {
        parsnip::survival_reg() |>
          parsnip::set_engine("flexsurvspline", k = k, scale = scale) |>
          pfit(
            formula = fit_formula,
            data = data
          )
      })

      names(models) <- purrr::pmap_chr(combinations, function(k, scale) {
        paste(k, "knot", scale, "scale", sep = "_")
      })

      distributions <- list(
        dists_attempted = combinations,
        dists_success = models |> purrr::discard(is.null) |> names(),
        dists_failed = models |> purrr::keep(is.null) |> names()
      )
    } else {
      models <- purrr::map(
        purrr::set_names(dists, dists), ~ {
          parsnip::survival_reg(dist = .x) |>
            parsnip::set_engine(engine) |>
            pfit(
              formula = fit_formula,
              data = data
            )
        }
      )

      distributions <- list(
        dists_attempted = dists,
        dists_success = models |> purrr::discard(is.null) |> names(),
        dists_failed = models |> purrr::keep(is.null) |> names()
      )
    }

    models <- models |> purrr::discard(is.null)

    parameters <- get_surv_parameters(models)

    summary <- list(
      fit_averages =
        data.table::rbindlist(lapply(models,
                                     get_fit_averages,
                                     get_mean = FALSE
        )) |> tibble::as_tibble()
    )

    # NOTE TO SELF: CAN TIDY THIS UP BY RETURNING FIRST THEN PUTTING IN SUMMARY?
    summary <- c(
      summary,
      list(goodness_of_fit = get_goodness_of_fit(models))
    )
  }

  if (approach == "separate_fits") {
    group_list <- levels(droplevels(as.factor(data[[group]])))
    nested <- data |> tidyr::nest(.by = group)

    for (tx in seq_along(group_list)) {
      data_subset <- nested[["data"]][[tx]]

      if (engine == "flexsurvspline") {
        combinations <- tidyr::expand_grid(k, scale)

        models[[tx]] <- purrr::pmap(combinations, function(k, scale) {
          parsnip::survival_reg() |>
            parsnip::set_engine("flexsurvspline", k = k, scale = scale) |>
            pfit(
              formula = fit_formula,
              data = data_subset
            )
        })

        names(models[[tx]]) <- purrr::pmap_chr(combinations, function(k, scale) {
          paste(k, "knot", scale, "scale", sep = "_")
        })

        distributions[[tx]] <- list(
          dists_attempted = combinations,
          dists_success = models[[tx]] |> purrr::discard(is.null) |> names(),
          dists_failed = models[[tx]] |> purrr::keep(is.null) |> names()
        )
      } else {
        models[[tx]] <- purrr::map(
          purrr::set_names(dists, dists), ~ {
            parsnip::survival_reg(dist = .x) |>
              parsnip::set_engine(engine) |>
              pfit(
                formula = fit_formula,
                data = data_subset
              )
          }
        )

        distributions[[tx]] <- list(
          dists_attempted = dists,
          dists_success = models[[tx]] |> purrr::discard(is.null) |> names(),
          dists_failed = models[[tx]] |> purrr::keep(is.null) |> names()
        )
      }

      models[[tx]] <- models[[tx]] |> purrr::discard(is.null)

      parameters[[tx]] <- get_surv_parameters(models[[tx]])

      summary[[tx]] <- list(
        fit_averages =
          data.table::rbindlist(lapply(models[[tx]],
                                       get_fit_averages,
                                       get_mean = FALSE
          )) |> tibble::as_tibble()
      )

      summary[[tx]] <- c(
        summary[[tx]],
        list(goodness_of_fit = get_goodness_of_fit(models[[tx]]))
      )
    }

    names(models) <-
      names(distributions) <-
      names(parameters) <-
      names(summary) <-
      group_list
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
    parameters = parameters,
    predictions = predictions,
    plots = plots,
    summary = summary,
    KM = KM,
    KM_plot = KM_plot
  )

  return(out)
}
