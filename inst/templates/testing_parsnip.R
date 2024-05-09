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
                     engine = "flexsurv") {
  formula <- as.formula(paste0(
    "survival::Surv(time = ",
    time,
    ", event = ",
    event,
    ") ~",
    covariates
  ))

  fits <- list()

  if (!is.null(repeat_for)) {
    strata_list <- levels(droplevels(as.factor(data[[repeat_for]])))
    nested <- data |> tidyr::nest(.by = repeat_for)

    for (tx in seq_along(strata_list)) {
      data_subset <- nested[["data"]][[tx]]

      models_list <- purrr::map(
        purrr::set_names(dists, dists), ~ {
          parsnip::survival_reg(dist = .x) |>
            parsnip::set_engine(engine) |>
            pfit(
              formula = formula,
              data = data_subset
            )
        }
      )

      fits[[tx]] <- models_list
    }

    names(fits) <- strata_list
  }

  if (is.null(repeat_for)) {
    fits <- purrr::map(
      purrr::set_names(dists, dists), ~ {
        parsnip::survival_reg(dist = .x) |>
          parsnip::set_engine(engine) |>
          pfit(
            formula = formula,
            data = data
          )
      }
    )
  }

  return(fits)
}


# when you want to repeat the model fitting for different groups, use repeat_for
output_repeats <- new_fits(
  data = surv_data,
  time = "time",
  event = "event",
  dists = dists,
  repeat_for = "group"
)
output_repeats

# when you want to specify a covariate, use covariates
output_joint <- new_fits(
  data = surv_data,
  time = "time",
  event = "event",
  dists = dists,
  covariates = "group"
)
output_joint

# when you want to fit all models to the same data, don't specify  repeat_for or covariates
output_all <- new_fits(
  data = surv_data,
  time = "time",
  event = "event",
  dists = dists
)
output_all
