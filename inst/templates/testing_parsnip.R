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
                     subset_on = NULL,
                     predict_for = NULL,
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
  # note if subset_on is NULL, it is dropped.
  required_cols <- c(time, event, subset_on)

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

  ## Check subset_on ----
  #...

  # Check that covariates don't contain what's in subset_on

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

  ## No sub-setting ----
  if (is.null(subset_on)) {
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

    models <- models |> purrr::discard(is.null)

    if (is.null(predict_for)) {

      # If predicting across whole dataset
      predictions <- lapply(models, predict,
                             new_data = data,
                             type = "survival",
                             eval_time = eval_time) |>
        purrr::map(~ .x |>
                     slice(1) |>
                     tidyr::unnest(col = .pred))

      names(predictions) <- names(models)

    } else {

      predict_list <- levels(droplevels(as.factor(data[[predict_for]])))

      for (looper_pred in seq_along(predict_list)) {

        predict_for_subset <- data |> dplyr::filter((!!rlang::sym(predict_for)) == predict_list[looper_pred])

        predictions[[looper_pred]] <- lapply(models, predict,
                                       new_data = predict_for_subset,
                                       type = "survival",
                                       eval_time = eval_time) |>
          purrr::map(~ .x |>
                       slice(1) |>
                       tidyr::unnest(col = .pred))
      }
    }
  }


  ## Sub-setting ----
  if (!is.null(subset_on)) {
    subset_list <- levels(droplevels(as.factor(data[[subset_on]])))
    nested <- data |> tidyr::nest(.by = subset_on)

    for (looper_sub in seq_along(subset_list)) {
      data_subset <- nested[["data"]][[looper_sub]]

      models[[looper_sub]] <- purrr::map(
        purrr::set_names(dists, dists), ~ {
          parsnip::survival_reg(dist = .x) |>
            parsnip::set_engine(engine) |>
            pfit(
              formula = formula,
              data = data_subset
            )
        }
      )

      distributions[[looper_sub]] <- list(dists_attempted = dists,
                                  dists_success = names(models[[looper_sub]])[!is.null(models[[looper_sub]])],
                                  dists_failed = names(models[[looper_sub]])[is.null(models[[looper_sub]])])

      models[[looper_sub]] <- models[[looper_sub]] |> purrr::discard(is.null)

      if (is.null(predict_for)) {

        predictions[[looper_sub]] <- lapply(models[[looper_sub]], predict,
                              new_data = data_subset,
                              type = "survival",
                              eval_time = eval_time) |>
          purrr::map(~ .x |>
                       slice(1) |>
                       tidyr::unnest(col = .pred))

        names(predictions) <- names(models)
      } else {

        predict_list <- levels(droplevels(as.factor(data[[predict_for]])))

        for (looper_pred in seq_along(predict_list)) {
          predict_for_subset <- data |> dplyr::filter((!!rlang::sym(predict_for)) == predict_list[looper_pred])

          predictions[[looper_sub]][[looper_pred]] <- lapply(models[[looper_sub]], predict,
                                      new_data = predict_for_subset,
                                      type = "survival",
                                      eval_time = eval_time) |>
            purrr::map(~ .x |>
                         slice(1) |>
                         tidyr::unnest(col = .pred))
        }

      } # end of models fitting loop


    }

    names(models) <-
      names(distributions) <- subset_list
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


# when you want to repeat the model fitting for different groups, use subset_on

output_stratified <- new_fits(
  data = surv_data,
  time = "time",
  event = "event",
  dists = dists,
  subset_on = "group",
  predict_for = "group"
)

output_stratified <- new_fits(
  data = surv_data,
  time = "time",
  event = "event",
  dists = dists,
  covariates = 1,
  subset_on = "group"
)

# If you wanted separate fits:
# covariates = 1, subset_on = "group"
# In this situation, you would want predictions for each group


# If you wanted joint fits:
# covariates = "group", subset_on = NULL
# In this situation, you would want predictions for each group
#







predict_list <- levels(droplevels(as.factor(surv_data[["group"]])))
my_predictions <- list()
for (looper_pred in seq_along(predict_list)) {
  predict_for_subset <- surv_data |> dplyr::filter(group == predict_list[looper_pred])

  my_predictions[[looper_pred]] <- lapply(output_stratified$models, predict,
                              new_data = predict_for_subset,
                              type = "survival",
                              eval_time = c(1,2,3)) |>
    purrr::map(~ .x |>
                 slice(1) |>
                 tidyr::unnest(col = .pred))
}

names(my_predictions) <- names(predict_list)


output_stratified <- new_fits(
  data = surv_data,
  time = "time",
  event = "event",
  dists = dists,
  subset_on = "group"
)

output_stratified <- new_fits(
  data = surv_data,
  time = "time",
  event = "event",
  dists = dists,
  subset_on = "group",
  predict_for = "group"
)

# when you want to specify a covariate, use covariates
output_joint <- new_fits(
  data = surv_data,
  time = "time",
  event = "event",
  dists = dists,
  covariates = "group",
  predict_for = "group"
)

# when you want to fit all models to the same data, don't specify  subset_on or covariates
output_all <- new_fits(
  data = surv_data,
  time = "time",
  event = "event",
  dists = dists
)


example_model <- parsnip::survival_reg(dist = "weibull") |>
  parsnip::set_engine("flexsurv") |>
  pfit(
    formula = survival::Surv(time = time, event = event) ~ 1,
    data = surv_data
  )

some_predictions <- predict(example_model,
        surv_data,
        type = "survival",
        eval_time = c(1, 2, 3))

one_prediction <- some_predictions |>
  slice(1) |>
  tidyr::unnest(col = .pred)



example_model2 <- parsnip::survival_reg(dist = "weibull") |>
  parsnip::set_engine("flexsurv") |>
  pfit(
    formula = survival::Surv(time = time, event = event) ~ group,
    data = surv_data
  )

some_predictions <- predict(example_model2,
                            surv_data,
                            type = "survival",
                            eval_time = c(1, 2, 3))



lapply(list(example_model), predict,
       new_data = surv_data,
       type = "survival",
       eval_time = c(1, 2, 3)) |>
  purrr::map(~ .x |>
               slice(1) |>
               tidyr::unnest(col = .pred))


lapply(list(example_model), predict,
       new_data = surv_data,
       type = "survival",
       eval_time = c(1, 2, 3)) |>
  purrr::map(~ .x |>
               slice(1) |>
               tidyr::unnest(col = .pred))

one_prediction <- some_predictions |>
  slice(1) |>
  tidyr::unnest(col = .pred)
