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
surv_data2 <- surv_data[c(5:6, 269:270),]

dists <- c("gengamma", "weibull", "exponential")

# define new functions
pfit <- purrr::possibly(.f = parsnip::fit)
new_fits <- function(data,
                     time = "time",
                     event = "event",
                     group = NULL,
                     group_as_covariate = FALSE,
                     dists,
                     eval_time = NULL,
                     engine = "flexsurv") {

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

  #...

  ## Check time ----
  #...

  ## Check event ----
  #...

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
    approach <- "joint_fit"
    covariate <- group
  }

  if (!is.null(group) & !group_as_covariate) {
    approach <- "separate_fit"
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
  #...

  ## Check engine ----
  #...


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

  if (approach == "no_groups" | approach == "joint_fit") {
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

    distributions <- list(dists_attempted = dists,
                          dists_success = models |> purrr::discard(is.null) |> names(),
                          dists_failed = models |> purrr::keep(is.null) |> names())

    models <- models |> purrr::discard(is.null)
  }

  if (approach == "separate_fit") {

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

      distributions[[tx]] <- list(dists_attempted = dists,
                                          dists_success = models[[tx]] |> purrr::discard(is.null) |> names(),
                                          dists_failed = models[[tx]] |> purrr::keep(is.null) |> names())


      models[[tx]] <- models[[tx]] |> purrr::discard(is.null)
    }

    names(models) <-
      names(distributions) <-
      group_list
  }


  # Predict models ----

  if (approach == "no_groups") {

    predictions <- lapply(models, predict,
                          new_data = data,
                          type = "survival",
                          eval_time = eval_time) |>
      purrr::map(~ .x |>
                   slice(1) |>
                   tidyr::unnest(col = .pred))

    names(predictions) <- names(models)

  }

  if (approach == "joint_fit") {

    for (tx in seq_along(group_list)) {

      predictions[[tx]] <- lapply(models, predict,
                                           new_data = data.frame(group = group_list[tx]),
                                           type = "survival",
                                           eval_time = eval_time) |>
        purrr::map(~ .x |>
                     slice(1) |>
                     tidyr::unnest(col = .pred))

    }

    names(predictions) <- group_list
  }

  if (approach == "separate_fit") {

    for (tx in seq_along(group_list)) {

      predictions[[tx]] <- lapply(models[[tx]], predict,
                                          new_data = data.frame(group = group_list[tx]),
                                          type = "survival",
                                          eval_time = eval_time) |>
        purrr::map(~ .x |>
                     slice(1) |>
                     tidyr::unnest(col = .pred))

    }
    names(predictions) <- names(models)
  }


  # Return ----
  out <- list(
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
  group_as_covariate = FALSE
)

# joint fits
output_joint <- new_fits(
  data = surv_data,
  time = "time",
  event = "event",
  dists = dists,
  group = "group",
  group_as_covariate = TRUE
)

# group excluded
output_no_groups <- new_fits(
  data = surv_data,
  time = "time",
  event = "event",
  dists = dists
)
