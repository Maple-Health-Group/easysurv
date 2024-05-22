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
dists <- c("gengamma", "weibull", "exp", "lognorm", "llog", "gamma", "gompertz")

dists_survival_engine <- names(survival::survreg.distributions)[1:8] # predicting from t dist not available yet (throws error)
names(flexsurv::flexsurv.dists)

tidy_predict_survival <- function(models,
                                  new_data,
                                  eval_time,
                                  interval = "none") {
  # Start with NULLs to make dropping them easy with c()

  list_pred_surv <-
    list_pred_hazard <-
    table_pred_surv <-
    table_pred_surv_lower <-
    table_pred_surv_upper <-
    table_pred_hazard <- NULL

  # make the predictions (survival)
  list_pred_surv <- lapply(models,
    predict,
    new_data = new_data,
    type = "survival",
    eval_time = eval_time,
    interval = interval
  ) |>
    purrr::map(~ .x |>
      slice(1) |>
      tidyr::unnest(col = .pred))

  # inner function to extract predictions into a table
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

  # Extract to summary tables
  table_pred_surv <- extract_predictions(list_pred_surv, ".pred_survival")
  if (interval == "confidence" & models[[1]]$spec$engine != "survival") {
    table_pred_surv_lower <- extract_predictions(list_pred_surv, ".pred_lower")
    table_pred_surv_upper <- extract_predictions(list_pred_surv, ".pred_upper")
  }


  # make the predictions (hazard)
  list_pred_hazard <- lapply(models,
    predict,
    new_data = new_data,
    type = "hazard",
    eval_time = eval_time,
    interval = interval
  ) |>
    purrr::map(~ .x |>
      slice(1) |>
      tidyr::unnest(col = .pred))

  table_pred_hazard <- extract_predictions(list_pred_hazard, ".pred_hazard")

  out <- c(
    list(list_pred_surv = list_pred_surv),
    list(list_pred_hazard = list_pred_hazard),
    list(table_pred_surv = table_pred_surv),
    list(table_pred_surv_lower = table_pred_surv_lower),
    list(table_pred_surv_upper = table_pred_surv_upper)
  )

  return(out)
}

plot_fits2 <- function(data) {
  # Pivot_longer so that ggplot2 is happy (requires data frame)
  long_data <- tidyr::pivot_longer(data,
    cols = -".eval_time",
    names_to = "Model",
    values_to = "Survival"
  )

  p <- ggplot(data = long_data, aes(x = .eval_time, y = Survival))
  p <- p + geom_line(aes(color = Model, group = Model))
  p <- p + labs(
    x = "Time",
    y = "Survival",
    color = ifelse(length(unique(long_data$Model)) == 1, "Model", "Models")
  )
  p <- p + theme_bw()

  return(p)
}

plot_km2 <- function(data) {



}


get_survival_parameters <- function(models) {
  # Initialize an empty list to store results
  surv_params <- list()

  for (i in seq_along(models)) {
    engine <- models[[i]]$spec$engine

    distribution <- switch(engine,
      "flexsurv" = models[[i]]$fit$dlist$name,
      "flexsurvspline" = names(models[i]),
      "survival" = models[[i]]$fit$dist,
      stop("Unknown engine type")
    )

    if (engine == "flexsurv" | engine == "flexsurvspline") {
      # Get parameters from res.t
      get_parameters <- models[[i]]$fit$res.t |>
        as.data.frame() |>
        tibble::rownames_to_column(var = "parameter")

      # Get vcov matrix
      get_vcov <- models[[i]]$fit$cov |>
        as.data.frame()

      # Make the column names consistent between models (v1, v2, v3, ...)
      colnames(get_vcov) <- paste0("v", seq_along(models[[i]]$fit$coefficients))

      # Combine all results
      combined_results <- cbind(distribution, get_parameters, get_vcov)

      # Track location for covariate parameter
      combined_results$covariate_marker <- combined_results$parameter
      if (!is.null(models[[i]]$fit$covpars)) {
        combined_results$covariate_marker[models[[i]]$fit$covpars] <- models[[i]]$fit$dlist$location
      }
    } else if (engine == "survival") {
      # With the survival package, it's a bit tricky.
      # Get number of parameters using degrees of freedom
      par_length <- length(models[[i]]$fit$df)

      # Get initial parameters from coefficients
      get_parameters <- models[[i]]$fit$coefficients |>
        as.data.frame() |>
        tibble::rownames_to_column(var = "parameter")
      colnames(get_parameters) <- c("parameter", "est")

      # If there's an additional parameter not included in coefficients, add it
      if (tail(names(models[[i]]$fit$icoef), 1) != tail(names(models[[i]]$fit$coefficients), 1)) {
        get_additional_parameters <- tail(models[[i]]$fit$icoef, 1) |>
          as.data.frame() |>
          tibble::rownames_to_column(var = "parameter")
        colnames(get_additional_parameters) <- c("parameter", "est")

        get_parameters <- rbind(get_parameters, get_additional_parameters)
      }

      # Get vcov matrix
      get_vcov <- models[[i]]$fit$var |>
        as.data.frame()

      # Make the column names consistent between models (v1, v2, v3, ...)
      colnames(get_vcov) <- paste0("v", seq_along(get_parameters$parameter))

      # Combine all results
      combined_results <- cbind(distribution, get_parameters, get_vcov)

      # Track location for covariate parameter (not reported in survival output)
      combined_results$covariate_marker <- "NR"
    }

    # Variance-covariance matrices
    surv_params[[i]] <- combined_results
  }

  # Start the tibble with distribution, parameter and covariate_marker columns.
  surv_params <- dplyr::bind_rows(surv_params) |>
    dplyr::relocate("distribution", "parameter", "covariate_marker") |>
    as_tibble()

  return(surv_params)
}


get_fit_averages2 <- function(mod,
                              get_median = TRUE,
                              get_rmst = TRUE,
                              get_mean = FALSE) {
  if (!get_median & !get_rmst & !get_mean) {
    stop("You need to include at least one average (median, rmst, or mean) in the get_fit_averages function")
  }

  out <- list()

  engine <- mod$spec$engine

  if (engine == "flexsurv" | engine == "flexsurvspline") {
    # Checking if the distribution is a spline model.
    distribution <- `if`(is.null(mod$fit$k), mod$fit$dlist$name, paste(
      mod$fit$k,
      "knot",
      mod$fit$scale
    ))

    # MEDIAN
    if (get_median) {
      median <- tryCatch(
        {
          summary(mod$fit, type = "median")
        },
        error = function(e) {
          message(paste(
            "warning:",
            distribution,
            "median survival time not calculable."
          ))
          return(list(as.data.frame(list("est" = "-", "lcl" = "-", "ucl" = "-"))))
        }
      )
    }

    # RESTRICTED MEAN
    if (get_rmst) {
      restricted_mean <- tryCatch(
        {
          summary(mod$fit, type = "rmst")
        },
        error = function(e) {
          message(paste(
            "warning:",
            distribution,
            "restricted mean survival time not calculable."
          ))
          return(list(as.data.frame(list("est" = "-", "lcl" = "-", "ucl" = "-"))))
        }
      )
    }

    # MEAN (most likely to fail)
    if (get_mean) {
      mean <- tryCatch(
        {
          summary(mod$fit, type = "mean")
        },
        error = function(e) {
          message(paste(
            "warning:",
            distribution,
            "mean survival time not calculable",
            "(likely due to a plateau in survival predictions)"
          ))
          return(list(as.data.frame(list("est" = "-", "lcl" = "-", "ucl" = "-"))))
        }
      )
    }

    if (get_median) {
      myseq <- median
    } else if (get_rmst) {
      myseq <- restricted_mean
    } else {
      myseq <- mean
    }

    # In case of strata, use seq_along
    for (i in seq_along(myseq)) {
      if (get_median) {
        median[[i]] <- median[[i]] |>
          dplyr::rename_with(~ paste0("median.", .x))
      }

      if (get_rmst) {
        restricted_mean[[i]] <- restricted_mean[[i]] |>
          dplyr::rename_with(~ paste0("rmst.", .x))
      }

      if (get_mean) {
        mean[[i]] <- mean[[i]] |>
          dplyr::rename_with(~ paste0("mean.", .x))
      }
    }



    # Combine into list per strata
    for (i in seq_along(myseq)) {
      # out[[i]] <- distribution
      out[[i]] <- data.frame(distribution = distribution)

      if (!is.null(names(myseq)[i])) {
        strata <- names(myseq)[i]
        out[[i]] <- cbind(
          out[[i]],
          strata
        )
      }

      if (get_median) {
        out[[i]] <- cbind(
          out[[i]],
          median[[i]]
        )
      }

      if (get_rmst) {
        out[[i]] <- cbind(
          out[[i]],
          restricted_mean[[i]]
        )
      }

      if (get_mean) {
        out[[i]] <- cbind(
          out[[i]],
          mean[[i]]
        )
      }
    }

    names(out) <- names(myseq)

    out <- data.table::rbindlist(out)
  } else if (engine == "survival") {
    distribution <- mod$fit$dist

    # Check for groups
    if (!is.null(mod$fit$xlevels)) {
      n_xlevels <- length(mod$fit$xlevels)

      for (i in seq_along(n_xlevels)) {
        out[[i]] <- data.frame(distribution = distribution)

        # Create a single row data frame for prediction data
        new_data <- data.frame(column1 = mod$fit$xlevels[[1]][i])
        # Rename the column to match that in the original data
        colnames(new_data) <- names(mod$fit$xlevels)

        # Get the median
        median[[i]] <- predict(mod$fit,
          newdata = new_data,
          type = "quantile",
          p = c(0.5)
        )

        out[[i]] <- cbind(out[[i]], median[[i]])
      }

      out <- data.table::rbindlist(out)
    } else {
      out <- data.frame(distribution = distribution)

      # Get the median (newdata does not matter)
      median <- predict(mod$fit,
        newdata = data.frame(testing = 123),
        type = "quantile",
        p = c(0.5)
      )

      out <- cbind(out, median)
    }
  }

  return(out)
}

get_goodness_of_fit <- function(mod) {
  # Get AIC and BIC values using stats:: because engine=survival doesn't record these
  AIC_values <- sapply(mod, function(x) stats::AIC(x$fit))
  BIC_values <- sapply(mod, function(x) stats::BIC(x$fit))

  AIC_ranks <- rank(AIC_values)
  BIC_ranks <- rank(BIC_values)

  out <- tibble::tibble(
    "dist" = names(mod),
    "AIC" = AIC_values,
    "BIC" = BIC_values,
    "AIC_rank" = AIC_ranks,
    "BIC_rank" = BIC_ranks
  )

  out[order(out$dist), ]
}



# define new functions
new_fits <- function(data,
                     time,
                     event,
                     group = NULL,
                     group_as_covariate = FALSE,
                     dists = c("exp", "gamma", "gengamma", "gompertz", "llog", "lognorm", "weibull"),
                     eval_time = NULL,
                     engine = "flexsurv",
                     k = c(1, 2, 3),
                     scale = c("hazard"),
                     include_ci = FALSE) {
  # Create supporting pfit ----
  pfit <- purrr::possibly(.f = parsnip::fit)

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

    parameters <- get_survival_parameters(models)

    summary <- list(
      fit_averages =
        data.table::rbindlist(lapply(models,
          get_fit_averages2,
          get_mean = FALSE
        )) |> as_tibble()
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

      parameters[[tx]] <- get_survival_parameters(models[[tx]])

      summary[[tx]] <- list(
        fit_averages =
          data.table::rbindlist(lapply(models[[tx]],
            get_fit_averages2,
            get_mean = FALSE
          )) |> as_tibble()
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


  # Predict models and create plots ----

  if (include_ci) {
    interval <- "confidence"
  } else {
    interval <- "none"
  }

  if (approach == "no_groups") {
    predictions <- tidy_predict_survival(
      models = models,
      new_data = data,
      eval_time = eval_time,
      interval = interval
    )

    plots <- plot_fits2(predictions$table_pred_surv)
  }

  if (approach == "joint_fits") {
    for (tx in seq_along(group_list)) {
      predictions[[tx]] <- tidy_predict_survival(
        models = models,
        new_data = data.frame(group = group_list[tx]),
        eval_time = eval_time,
        interval = interval
      )

      plots[[tx]] <- plot_fits2(predictions[[tx]]$table_pred_surv)
    }

    names(predictions) <- names(plots) <- group_list
  }

  if (approach == "separate_fits") {
    for (tx in seq_along(group_list)) {
      predictions[[tx]] <- tidy_predict_survival(
        models = models[[tx]],
        new_data = data.frame(group = group_list[tx]),
        eval_time = eval_time,
        interval = interval
      )

      plots[[tx]] <- plot_fits2(predictions[[tx]]$table_pred_surv)
    }

    names(predictions) <- names(plots) <- names(models)
  }

  # Create plots ----

  # ...


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

# splines
output_separate_spline <- new_fits(
  data = surv_data,
  time = "time",
  event = "event",
  dists = dists,
  group = "group",
  engine = "flexsurvspline",
  k = c(1, 2, 3),
  scale = c("hazard", "odds"),
  group_as_covariate = FALSE,
  include_ci = TRUE
)

# using the "survival" engine
output_separate_diff_engine <- new_fits(
  data = surv_data,
  time = "time",
  event = "event",
  dists = dists_survival_engine,
  engine = "survival",
  group = "group",
  group_as_covariate = FALSE,
  include_ci = FALSE
)

#
# get_survival_parameters(output_joint$models)
# get_goodness_of_fit(output_separate[["models"]][["Good"]])
# plot_fits2(output_separate$predictions$Good$table_pred_surv)
#
# pfit <- purrr::possibly(.f = parsnip::fit)
# diff_models <- purrr::map(
#   purrr::set_names(dists_survival_engine, dists_survival_engine), ~ {
#     parsnip::survival_reg(dist = .x) |>
#       parsnip::set_engine("survival") |>
#       pfit(
#         formula = survival::Surv(time = time, event = event) ~ group,
#         data = surv_data
#       )
#   }
# )
#
# diff_models_sep <- purrr::map(
#   purrr::set_names(dists_survival_engine, dists_survival_engine), ~ {
#     parsnip::survival_reg(dist = .x) |>
#       parsnip::set_engine("survival") |>
#       pfit(
#         formula = survival::Surv(time = time, event = event) ~ 1,
#         data = surv_data
#       )
#   }
# )

#
# output_separate_diff_engine <- new_fits(
#   data = surv_data,
#   time = "time",
#   event = "event",
#   dists = dists_survival_engine,
#   engine = "survival",
#   group = "group",
#   group_as_covariate = FALSE,
#   include_ci = FALSE
# )
#
#
# spline_models <- purrr::map(
#   purrr::set_names(dists, dists), ~ {
#     parsnip::survival_reg(dist = .x) |>
#       parsnip::set_engine("flexsurvspline", k = 1) |>
#       pfit(
#         formula = survival::Surv(time = time, event = event) ~ 1,
#         data = surv_data
#       )
#   }
# )
#
# spline_models2 <- purrr::map(
#   purrr::set_names(dists, dists), ~ {
#     parsnip::survival_reg(dist = .x) |>
#       parsnip::set_engine("flexsurvspline", k = 1, scale = "odds") |>
#       pfit(
#         formula = survival::Surv(time = time, event = event) ~ 1,
#         data = surv_data
#       )
#   }
# )
#
# not_spline_models <- purrr::map(
#   purrr::set_names(dists, dists), ~ {
#     parsnip::survival_reg(dist = .x) |>
#       parsnip::set_engine("flexsurv") |>
#       pfit(
#         formula = survival::Surv(time = time, event = event) ~ 1,
#         data = surv_data
#       )
#   }
# )
#
#
# my_engine <- "flexsurv"
#
#
# not_spline_models <- purrr::map(
#   purrr::set_names(dists, dists), ~ {
#     parsnip::survival_reg(dist = .x) |>
#       dplyr::case_when(
#         my_engine == "flexsurv" ~ parsnip::set_engine("flexsurv"),
#         my_engine == "flexsurvspline" ~ parsnip::set_engine("flexsurvspline", k = 1)) |>
#       pfit(
#         formula = survival::Surv(time = time, event = event) ~ 1,
#         data = surv_data
#       )
#   }
# )
#
#
#
#
#
#
#
#
#
#
#
#
# k <- 1
# scale <- "hazard"
#
#
# testing123
#
# testing123names <- purrr::pmap_chr(testing123, function(k, scale) {
#   paste("knot", k, "scale", scale, sep = "_")
# })
#
#
#
# # This section to keep:
#
# k <- c(1,2,3)
# scale <- c("odds", "hazard")
# combinations <- tidyr::expand_grid(k, scale)
#
# testing_output <- purrr::pmap(combinations, function(k, scale) {
#   parsnip::survival_reg() |>
#     parsnip::set_engine("flexsurvspline", k = k, scale = scale) |>
#     pfit(
#       formula = survival::Surv(time = time, event = event) ~ 1,
#       data = surv_data
#     )
# })
#
# names(testing_output) <- purrr::pmap_chr(combinations, function(k, scale) {
#   paste(k, "knot", scale, "scale", sep = "_")
# })
# ####################
#
#
#
# testing_output <- purrr::pmap(purrr::set_names(testing123, testing123names), function(k, scale) {
#   parsnip::survival_reg() |>
#     parsnip::set_engine("flexsurvspline", k = k, scale = scale) |>
#     pfit(
#       formula = survival::Surv(time = time, event = event) ~ 1,
#       data = surv_data
#     )
# })
#
# purrr::set_names(testing123, testing123names)
#
# testing123
#
# # this works
# spline_models <- purrr::map(
#   purrr::set_names(my_knots, my_knots), ~ {
#     parsnip::survival_reg() |>
#       parsnip::set_engine("flexsurvspline", k = .x, scale = "odds") |>
#       pfit(
#         formula = survival::Surv(time = time, event = event) ~ 1,
#         data = surv_data
#       )
#   }
# )
