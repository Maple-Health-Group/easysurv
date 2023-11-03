# first test file for the summarise_KM() function.
test_that("output is correct", {
  fit <- survival::survfit(survival::Surv(time, status) ~ as.factor(sex), data = survival::lung)
  expect_equal({
    summarise_KM(fit)
  },
  readRDS(test_path("fixtures", "KM_summary.rds"))
  )
})
