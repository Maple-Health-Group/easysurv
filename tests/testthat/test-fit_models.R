test_that("models work without predict_by", {
  expect_no_error(
    fit_models(
      data = easy_lung,
      time = "time",
      event = "status",
      engine = "flexsurv"
    )
  )
})

test_that("separate models work", {
  expect_no_error(
    fit_models(
      data = easy_lung,
      time = "time",
      event = "status",
      predict_by = "sex",
      engine = "flexsurv"
    )
  )
})

test_that("joint models work", {
  expect_no_error(
    fit_models(
      data = easy_lung,
      time = "time",
      event = "status",
      predict_by = "sex",
      covariates = c("sex"),
      engine = "flexsurv"
    )
  )
})

test_that("cure models work", {
  expect_no_error(
    fit_models(
      data = easy_lung,
      time = "time",
      event = "status",
      predict_by = "sex",
      engine = "flexsurvcure"
    )
  )
})

test_that("spline models work", {
  expect_no_error(
    fit_models(
      data = easy_lung,
      time = "time",
      event = "status",
      predict_by = "sex",
      engine = "flexsurvspline"
    )
  )
})

test_that("separate models work with survival engine", {
  expect_no_error(
    fit_models(
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
  )
})

test_that("joint models work with survival engine", {
  expect_no_error(
    fit_models(
      data = easy_lung,
      time = "time",
      event = "status",
      predict_by = "sex",
      covariates = "sex",
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
  )
})
