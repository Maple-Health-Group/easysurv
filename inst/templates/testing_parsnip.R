rm(list = ls())

library(tidymodels)
library(censored)
library(purrr)

surv_data <- flexsurv::bc |>
  dplyr::mutate(
    time = recyrs,
    event = censrec
  )

dists <- c("gengamma", "weibull", "exponential")

# define new functions
pfit <- purrr::possibly(.f = parsnip::fit)
new_fits <- function(data,
                     time = "time",
                     event = "event",
                     covariates = 1,
                     repeat_for = NULL,
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
  # note if repeat_for is NULL, it is dropped.
  required_cols <- c(time, event, repeat_for)

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

  ## Check covariates ----
  #...

  ## Check repeat_for ----
  #...

  # Check that covariates don't contain what's in repeat_for

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
    covariates
  ))

  # Fit models ----

  ## Non-stratified models ----
  if (is.null(repeat_for)) {
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
                          dists_success = names(models)[!is.null(models)],
                          dists_failed = names(models)[is.null(models)])
  }


  ## Stratified models ----
  if (!is.null(repeat_for)) {
    repeat_list <- levels(droplevels(as.factor(data[[repeat_for]])))
    nested <- data |> tidyr::nest(.by = repeat_for)

    for (tx in seq_along(repeat_list)) {
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
                                  dists_success = names(models[[tx]])[!is.null(models[[tx]])],
                                  dists_failed = names(models[[tx]])[is.null(models[[tx]])])
    }

    names(models) <-
      names(distributions) <- repeat_list
  }

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


# when you want to repeat the model fitting for different groups, use repeat_for
output_repeats <- new_fits(
  data = surv_data,
  time = "time",
  event = "event",
  dists = dists,
  repeat_for = "group"
)

# when you want to specify a covariate, use covariates
output_joint <- new_fits(
  data = surv_data,
  time = "time",
  event = "event",
  dists = dists,
  covariates = "group"
)

# when you want to fit all models to the same data, don't specify  repeat_for or covariates
output_all <- new_fits(
  data = surv_data,
  time = "time",
  event = "event",
  dists = dists
)
