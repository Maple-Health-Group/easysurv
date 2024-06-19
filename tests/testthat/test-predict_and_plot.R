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
