# first test file for the get_results_table() function.
test_that("output is correct", {
  fit <- survHE::fit.models(survival::Surv(time, status) ~ as.factor(sex),
                            data = survival::lung,
                            distr = c("exponential", "weibull")
  )
  expect_equal({
    get_results_table(fit$models)
  },
  readRDS(test_path("fixtures", "fit_results_table.rds"))
  )
})
