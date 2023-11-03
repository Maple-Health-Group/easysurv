# first test file for the get_fit_vcov() function.
test_that("output is correct", {
  fit <- flexsurv::flexsurvreg(survival::Surv(time, status) ~ as.factor(sex),
                               data = survival::lung,
                               dist = "exponential"
  )
  expect_equal({
    get_fit_vcov(fit)
  },
  readRDS(test_path("fixtures", "fit_vcov.rds"))
  )
})
