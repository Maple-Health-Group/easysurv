# first test file for the get_fit_comparison() function.
test_that("output is correct", {
  fit <- survHE::fit.models(survival::Surv(time, status) ~ as.factor(sex),
                    data = survival::lung,
                    distr = c("exponential", "weibull")
  )
  expect_equal({
    get_fit_comparison(fit)
  },
  readRDS(test_path("fixtures", "fit_comparison.rds"))
  )
})
