rm(list = ls())

library(tidymodels)
library(censored)
library(purrr)

surv_data <- flexsurv::bc |>
  dplyr::mutate(
    time = recyrs,
    event = censrec
  )

# For testing purposes, a data set that will fail.
surv_data2 <- surv_data[c(5:6, 269:270), ]

dists <- c("gengamma", "weibull", "exponential")
dists <- c("gengamma", "weibull", "exp", "lognorm", "llog", "gamma")

extract_predictions <- function(pred_list, col_name) {
  Reduce(
    function(x, y) merge(x, y, by = ".eval_time", all = TRUE),
    (lapply(names(pred_list), function(model) {
      df <- pred_list[[model]][, c(".eval_time", col_name)]
      colnames(df)[2] <- model
      return(df)
    }))
  ) |> as_tibble()
}

get_survival_parameters <- function(models) {
  # Initialize an empty list to store results
  surv_params <- list()

  for (i in seq_along(models)) {
    # Set the distribution name. This is based on dlist so that, for example:
    # "exponential" and "exp" both become "exp"
    distribution <- models[[i]]$fit$dlist$name

    # Get parameters from res.t
    get_parameters <- models[[i]]$fit$res.t |>
      as.data.frame() |>
      tibble::rownames_to_column(var = "parameter")

    # Get vcov matrix from cov
    get_vcov <- models[[i]]$fit$cov |>
      as.data.frame()

    # Make the column names consistent between models (v1, v2, v3, ...)
    colnames(get_vcov) <- paste0("v", seq_along(models[[i]]$fit$coefficients))

    # Track covariates
    get_vcov$covariate_marker <- NA
    if (!is.null(models[[i]]$fit$covpars)) {
      get_vcov$covariate_marker[models[[i]]$fit$covpars] <- "covariate"
    }

    # Combine all results
    combined_results <- cbind(distribution, get_parameters, get_vcov)

    # Assign covariate_marker based on conditions
    # Helps the user know how to treat covariates when applying surv params.
    combined_results$covariate_marker <- dplyr::case_when(
      (distribution %in% c(
        "exp",
        "gamma",
        "gompertz"
      ) & (combined_results$covariate_marker == "covariate" |
        combined_results$parameter == "rate")) ~ "rate",
      (distribution %in% c(
        "llogis",
        "weibull.quiet"
      ) & (combined_results$covariate_marker == "covariate" |
        combined_results$parameter == "scale")) ~ "scale",
      (distribution == "gengamma" &
        (combined_results$covariate_marker == "covariate" |
          combined_results$parameter == "mu")) ~ "mu",
      (distribution == "lnorm" &
        (combined_results$covariate_marker == "covariate" |
          combined_results$parameter == "meanlog")) ~ "meanlog",
      is.na(combined_results$covariate_marker) ~ combined_results$parameter
    )

    # Variance-covariance matrices
    surv_params[[i]] <- combined_results
  }

  # Start the tibble with distribution, parameter and covariate_marker columns.
  surv_params <- dplyr::bind_rows(surv_params) |>
    dplyr::relocate("distribution", "parameter", "covariate_marker") |>
    as_tibble()

  return(surv_params)
}

# define new functions
pfit <- purrr::possibly(.f = parsnip::fit)
new_fits <- function(data,
                     time,
                     event,
                     group = NULL,
                     group_as_covariate = FALSE,
                     dists,
                     eval_time = NULL,
                     engine = "flexsurv",
                     include_ci = FALSE) {
  # Create key objects ----
  distributions <- list()
  models <- list()
  parameters <- list()
  predictions <- list()
  plots <- list()
  summary <- list()

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
    covariate <- 1
  } else {
    group_list <- levels(droplevels(as.factor(data[[group]])))
  }

  if (!is.null(group) & group_as_covariate) {
    approach <- "joint_fits"
    covariate <- group
  }

  if (!is.null(group) & !group_as_covariate) {
    approach <- "separate_fits"
    covariate <- 1
  }


  ## Check dists ----
  # Check that dists isn't missing
  if (missing(dists)) {
    stop(
      "`dists` is missing.",
      call. = FALSE
    )
  }

  # Check that dists has legal values for engine
  # ...

  ## Check engine ----
  # ...


  # Create formula ----

  formula <- as.formula(paste0(
    "survival::Surv(time = ",
    time,
    ", event = ",
    event,
    ") ~",
    covariate
  ))

  # Fit models ----

  if (approach == "no_groups" | approach == "joint_fits") {
    models <- purrr::map(
      purrr::set_names(dists, dists), ~ {
        parsnip::survival_reg(dist = .x) |>
          parsnip::set_engine(engine) |>
          pfit(
            formula = formula,
            data = data
          )
      }
    )

    distributions <- list(
      dists_attempted = dists,
      dists_success = models |> purrr::discard(is.null) |> names(),
      dists_failed = models |> purrr::keep(is.null) |> names()
    )

    models <- models |> purrr::discard(is.null)

    parameters <- get_survival_parameters(models)
  }

  if (approach == "separate_fits") {
    group_list <- levels(droplevels(as.factor(data[[group]])))
    nested <- data |> tidyr::nest(.by = group)

    for (tx in seq_along(group_list)) {
      data_subset <- nested[["data"]][[tx]]

      models[[tx]] <- purrr::map(
        purrr::set_names(dists, dists), ~ {
          parsnip::survival_reg(dist = .x) |>
            parsnip::set_engine(engine) |>
            pfit(
              formula = formula,
              data = data_subset
            )
        }
      )

      distributions[[tx]] <- list(
        dists_attempted = dists,
        dists_success = models[[tx]] |> purrr::discard(is.null) |> names(),
        dists_failed = models[[tx]] |> purrr::keep(is.null) |> names()
      )


      models[[tx]] <- models[[tx]] |> purrr::discard(is.null)

      parameters[[tx]] <- get_survival_parameters(models[[tx]])
    }

    names(models) <-
      names(distributions) <-
      names(parameters) <-
      group_list
  }


  # Predict models ----

  if (include_ci) {
    interval <- "confidence"
  } else {
    interval <- "none"
  }

  if (approach == "no_groups") {

    predicted_survival <- lapply(models, predict,
      new_data = data,
      type = "survival",
      eval_time = eval_time,
      interval = interval
    ) |>
      purrr::map(~ .x |>
        slice(1) |>
        tidyr::unnest(col = .pred))

    # Extract to summary tables
    predictions <- list(pred_surv = extract_predictions(predicted_survival, ".pred_survival"))

    if (include_ci) {
      predictions <- c(
        predictions,
        list(pred_surv_lower = extract_predictions(predicted_survival, ".pred_lower")),
        list(pred_surv_upper = extract_predictions(predicted_survival, ".pred_upper"))
      )
    }

    predicted_hazard <- lapply(models, predict,
      new_data = data,
      type = "hazard",
      eval_time = eval_time,
      interval = interval
    ) |>
      purrr::map(~ .x |>
        slice(1) |>
        tidyr::unnest(col = .pred))

    predictions <- c(
      predictions,
      list(pred_hazard = extract_predictions(predicted_hazard, ".pred_hazard"))
    )
  }

  if (approach == "joint_fits") {
    for (tx in seq_along(group_list)) {
      # Create predictions (survival)
      predicted_survival <- lapply(models, predict,
        new_data = data.frame(group = group_list[tx]),
        type = "survival",
        eval_time = eval_time,
        interval = interval
      ) |>
        purrr::map(~ .x |>
          slice(1) |>
          tidyr::unnest(col = .pred))


      # Extract to summary tables
      predictions[[tx]] <- list(pred_surv = extract_predictions(predicted_survival, ".pred_survival"))

      if (include_ci) {
        predictions[[tx]] <- c(
          predictions[[tx]],
          list(pred_surv_lower = extract_predictions(predicted_survival, ".pred_lower")),
          list(pred_surv_upper = extract_predictions(predicted_survival, ".pred_upper"))
        )
      }

      # Create predictions (hazard)
      predicted_hazard <- lapply(models, predict,
        new_data = data.frame(group = group_list[tx]),
        type = "hazard",
        eval_time = eval_time,
        interval = interval
      ) |>
        purrr::map(~ .x |>
          slice(1) |>
          tidyr::unnest(col = .pred))


      # Extract to summary tables
      predictions[[tx]] <- c(
        predictions[[tx]],
        list(pred_hazard = extract_predictions(predicted_hazard, ".pred_hazard"))
      )
    }

    names(predictions) <- group_list
  }

  if (approach == "separate_fits") {
    for (tx in seq_along(group_list)) {
      # Create predictions (survival)
      predicted_survival <- lapply(models[[tx]], predict,
        new_data = data.frame(group = group_list[tx]),
        type = "survival",
        eval_time = eval_time,
        interval = interval
      ) |>
        purrr::map(~ .x |>
          slice(1) |>
          tidyr::unnest(col = .pred))

      # Extract to summary tables
      predictions[[tx]] <- list(pred_surv = extract_predictions(predicted_survival, ".pred_survival"))

      if (include_ci) {
        predictions[[tx]] <- c(
          predictions[[tx]],
          list(pred_surv_lower = extract_predictions(predicted_survival, ".pred_lower")),
          list(pred_surv_upper = extract_predictions(predicted_survival, ".pred_upper"))
        )
      }

      # Create predictions (hazard) - interval isn't an option in underlying predict function
      predicted_hazard <- lapply(models[[tx]], predict,
        new_data = data.frame(group = group_list[tx]),
        type = "hazard",
        eval_time = eval_time
      ) |>
        purrr::map(~ .x |>
          slice(1) |>
          tidyr::unnest(col = .pred))

      predictions[[tx]] <- c(
        predictions[[tx]],
        list(pred_hazard = extract_predictions(predicted_hazard, ".pred_hazard"))
      )
    }

    names(predictions) <- names(models)
  }


  # Return ----
  out <- list(
    approach = approach,
    distributions = distributions,
    models = models,
    parameters = parameters,
    predictions = predictions,
    plots = plots,
    summary = summary
  )

  return(out)
}

# bottom of function ----

# separate fits
output_separate <- new_fits(
  data = surv_data,
  time = "time",
  event = "event",
  dists = dists,
  group = "group",
  group_as_covariate = FALSE,
  include_ci = TRUE
)


# joint fits
output_joint <- new_fits(
  data = surv_data,
  time = "time",
  event = "event",
  dists = dists,
  group = "group",
  group_as_covariate = TRUE,
  include_ci = TRUE
)

# group excluded
output_no_groups <- new_fits(
  data = surv_data,
  time = "time",
  event = "event",
  dists = dists
)


get_survival_parameters(output_separate$models$Poor)
get_survival_parameters(output_joint$models)
