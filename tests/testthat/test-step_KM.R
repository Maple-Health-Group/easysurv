# first test file for the step_KM() function.
test_that("output is correct", {
  expect_equal({
    fit <- survival::survfit(survival::Surv(time, status) ~ 1, data = survival::lung[1:10,])
    step_KM(fit, add_time_0 = TRUE)
    },
    readRDS(test_path("fixtures", "step_KM_tibble.rds"))
  )
})
