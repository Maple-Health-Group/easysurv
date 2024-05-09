library(tidymodels)
library(censored)
library(purrr)

data(bc, package = "flexsurv")

my_subset <- bc[c(1:3,230:235),]

pfit <- purrr::possibly(.f = parsnip::fit)

dists <- c("gengamma", "weibull", "exponential")

models_list <- purrr::map(
  purrr::set_names(dists, dists), ~ {
    parsnip::survival_reg(dist = .x) |>
      parsnip::set_engine("flexsurv") |>
      pfit(formula = survival::Surv(time = recyrs,
                                    event = censrec) ~ group,
           data = my_subset)
  }
)

surv_data <- bc |>
  dplyr::mutate(
  time = recyrs,
  event = censrec,
  strata = group
)

times <- seq(
  from = 0,
  to = ceiling(max(surv_data$time) * 5),
  length.out = 200
)

# just a test that says we can use weights
case_weights_allowed(survival_reg(engine = "flexsurv"))




levels(surv_data$strata) <- strata_labels <- c("Good", "Medium", "Poor")

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
            pfit(formula = formula,
                 data = data_subset)
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
          pfit(formula = formula,
               data = data)
      }
    )
  }

  return(fits)

}

output_repeats <- new_fits(data = surv_data,
                           time = "time",
                           event = "event",
                           dists = dists,
                           repeat_for = "strata")

output_repeats

output_joint <- new_fits(data = surv_data,
                         time = "time",
                         event = "event",
                         dists = dists,
                         covariates = "group")

output_joint















nestedd <- surv_data |> dplyr::nest_by(.data[["strata"]])

tidyrnest <- surv_data |> tidyr::nest(.by = "strata")

unnest(tidyrnest, cols = c(data))

grouped <- surv_data |> dplyr::group_by(.data[["strata"]])



output_es <- easysurv::quick_fit(
  data = surv_data,
  time = "time",
  event = "event",
  strata = "strata",
  dists = dists,
  # Optional easysurv arguments
  times = times,
  strata_labels = strata_labels,
  xlab = "Months",
  add_interactive_plots = FALSE
)

