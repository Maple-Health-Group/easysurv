# first test file for the get_fit_coef() function.
test_that("output is correct", {
  fit <- flexsurv::flexsurvreg(survival::Surv(time, status) ~ as.factor(sex),
                     data = survival::lung,
                     dist = "exponential"
  )
  expect_equal({
    get_fit_coef(fit)
  },
  readRDS(test_path("fixtures", "fit_coefs.rds"))
  )
})
