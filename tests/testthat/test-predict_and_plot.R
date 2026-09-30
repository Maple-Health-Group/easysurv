test_that("separate models work in predict_and_plot()", {
  separate_models <- fit_models(
    data = easy_lung,
    time = "time",
    event = "status",
    predict_by = "sex"
  )

  expect_no_error(
    predict_and_plot(
      fit_models = separate_models
    )
  )
})

test_that("joint models work in predict_and_plot()", {
  # predict_by should use a factor variable
  test_data <- easy_lung
  test_data$sex <- as.factor(test_data$sex)

  joint_models <- fit_models(
    data = test_data,
    time = "time",
    event = "status",
    predict_by = "sex",
    covariates = "sex"
  )

  expect_no_error(
    predict_and_plot(
      fit_models = joint_models
    )
  )
})

test_that("separate models work with survival engine in predict_and_plot()", {
  surv_models <- fit_models(
    data = easy_lung,
    time = "time",
    event = "status",
    predict_by = "sex",
    dists = c(
      "exponential",
      "extreme",
      "gaussian",
      "logistic",
      "lognormal",
      "rayleigh",
      "weibull"
    ),
    engine = "survival"
  )

  expect_no_error(
    predict_and_plot(
      fit_models = surv_models
    )
  )
})

test_that("predict_and_plot() works when all strata have equal KM row counts (#27)", {
  # Two groups with the same number of distinct observed times. Previously,
  # mapply() returned a matrix rather than a list when building the KM group
  # column, which dropped the `group` column and broke dplyr::filter().
  n <- 60
  equal_strata_data <- data.frame(
    time = c(
      stats::qexp(stats::ppoints(n), rate = 0.10),
      stats::qexp(stats::ppoints(n), rate = 0.15)
    ),
    event = rep(c(1, 1, 1, 1, 0), length.out = 2 * n),
    group = factor(rep(c("A", "B"), each = n))
  )

  # Confirm the precondition that triggers the bug: equal rows per stratum.
  km <- survival::survfit(
    survival::Surv(time, event) ~ group,
    data = equal_strata_data
  )
  expect_length(unique(km$strata), 1)

  separate_models <- fit_models(
    data = equal_strata_data,
    time = "time",
    event = "event",
    predict_by = "group",
    dists = c("exp", "weibull")
  )

  expect_no_error(
    pred_separate <- predict_and_plot(fit_models = separate_models)
  )
  expect_length(pred_separate$plots, 2)

  joint_models <- fit_models(
    data = equal_strata_data,
    time = "time",
    event = "event",
    predict_by = "group",
    covariates = "group",
    dists = c("exp", "weibull")
  )

  expect_no_error(
    pred_joint <- predict_and_plot(fit_models = joint_models)
  )
  expect_length(pred_joint$plots, 2)
})
