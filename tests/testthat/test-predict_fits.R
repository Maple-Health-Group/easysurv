# first test file for the predict_fits() function.
test_that("output is correct", {
  fits <- survHE::fit.models(survival::Surv(time, status) ~ 1,
                             data = survival::lung,
                             dist = c("exponential", "weibull")
  )
  t <- seq(from = 0, to = 1000, length.out = 100)
  expect_equal({
    predict_fits(fits, t, group = 1)
  },
  readRDS(test_path("fixtures", "predicted_fits.rds"))
  )
})
