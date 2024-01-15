# first test file for the predict_fits() function.
test_that("output is correct", {
  fits <- survHE::fit.models(survival::Surv(time, status) ~ 1,
                             data = survival::lung,
                             dist = c("exponential", "weibull")
  )
  t <- c(1:50)
  expect_equal({
    set.seed(1234) # need to set seed since CI generation uses simulated data
    predict_fits(fits, t, group = 1)
  },
  readRDS(test_path("fixtures", "predicted_fits.rds"))
  )
})
