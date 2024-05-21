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

tidy_predict_survival <- function(models,
                                  new_data,
                                  eval_time,
                                  interval = "none") {

  # make the predictions (survival)
  predictions <- lapply(models,
    predict,
    new_data = new_data,
    type = "survival",
    eval_time = eval_time,
    interval = interval
  ) |>
    purrr::map(~ .x |>
      slice(1) |>
      tidyr::unnest(col = .pred))

  # inner function to extract predictions
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
  out <- list(pred_surv = extract_predictions(predictions, ".pred_survival"))

  if (interval == "confidence") {
    out <- c(
      out,
      list(pred_surv_lower = extract_predictions(predictions, ".pred_lower")),
      list(pred_surv_upper = extract_predictions(predictions, ".pred_upper"))
    )
  }

  # make the predictions (hazard)
  predictions <- lapply(models,
                        predict,
                        new_data = new_data,
                        type = "hazard",
                        eval_time = eval_time,
                        interval = interval
  ) |>
    purrr::map(~ .x |>
                 slice(1) |>
                 tidyr::unnest(col = .pred))


  out <- c(out,
           list(pred_hazard = extract_predictions(predictions, ".pred_hazard")))


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

    # Combine all results
    combined_results <- cbind(distribution, get_parameters, get_vcov)

    # Track location for covariate parameter
    combined_results$covariate_marker <- combined_results$parameter
    if (!is.null(models[[i]]$fit$covpars)) {
      combined_results$covariate_marker[models[[i]]$fit$covpars] <- models[[i]]$fit$dlist$location
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

  out <- list()

  # Combine into list per strata
  for (i in seq_along(myseq)) {

    #out[[i]] <- distribution
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

  return(out)
}

get_goodness_of_fit <- function(mod) {

  AIC_values <- sapply(mod, function(x) x$fit$AIC)
  BIC_values <- sapply(mod, function(x) x$fit$BIC)

  AIC_ranks <- rank(AIC_values)
  BIC_ranks <- rank(BIC_values)

  out <- tibble::tibble("dist" = names(mod),
                        "AIC" = AIC_values,
                        "BIC" = BIC_values,
                        "AIC_rank" = AIC_ranks,
                        "BIC_rank" = BIC_ranks)

  out[order(out$dist), ]
}



# define new functions
new_fits <- function(data,
                     time,
                     event,
                     group = NULL,
                     group_as_covariate = FALSE,
                     dists,
                     eval_time = NULL,
                     engine = "flexsurv",
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

    summary <- list(fit_averages =
      data.table::rbindlist(lapply(models,
                                   get_fit_averages2,
                                   get_mean = FALSE)) |> as_tibble()
    )

    # NOTE TO SELF: CAN TIDY THIS UP BY RETURNING FIRST THEN PUTTING IN SUMMARY?
    summary <- c(summary,
                 list(goodness_of_fit = get_goodness_of_fit(models)))


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

      summary[[tx]] <- list(fit_averages =
        data.table::rbindlist(lapply(models[[tx]],
                                     get_fit_averages2,
                                     get_mean = FALSE)) |> as_tibble()
      )

      summary[[tx]] <- c(summary[[tx]],
                   list(goodness_of_fit = get_goodness_of_fit(models[[tx]])))

    }

    names(models) <-
      names(distributions) <-
      names(parameters) <-
      names(summary) <-
      group_list
  }


  # Predict models ----

  if (include_ci) {
    interval <- "confidence"
  } else {
    interval <- "none"
  }

  if (approach == "no_groups") {

    predictions <- tidy_predict_survival(models = models,
                                         new_data = data,
                                         eval_time = eval_time,
                                         interval = interval)

  }

  if (approach == "joint_fits") {
    for (tx in seq_along(group_list)) {

      predictions[[tx]] <- tidy_predict_survival(models = models,
                                                  new_data = data.frame(group = group_list[tx]),
                                                  eval_time = eval_time,
                                                  interval = interval)
    }

    names(predictions) <- group_list
  }

  if (approach == "separate_fits") {
    for (tx in seq_along(group_list)) {

      predictions[[tx]] <- tidy_predict_survival(models = models[[tx]],
                                                  new_data = data.frame(group = group_list[tx]),
                                                  eval_time = eval_time,
                                                  interval = interval)

    }

    names(predictions) <- names(models)
  }

  # Create plots ----

  #...


  # Create summary ----




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

get_survival_parameters(output_joint$models)
get_goodness_of_fit(output_separate[["models"]][["Good"]])
