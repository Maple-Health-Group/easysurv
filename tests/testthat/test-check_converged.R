# first test file for the check_converged() function.
test_that("output is correct", {
  expect_equal({
    check_converged(
      formula = survival::Surv(time, status) ~ as.factor(sex),
      data = survival::lung[1:7,],
      dists = c("exponential", "weibull", "gengamma", "lognormal")
    )
  },
  readRDS(test_path("fixtures", "converged_standard.rds"))
  )
})

test_that("warning is correct", {
  expect_message({
    check_converged(
      formula = survival::Surv(time, status) ~ as.factor(sex),
      data = survival::lung[1:7,],
      dists = c("exponential", "weibull", "gengamma", "lognormal")
    )
  }, "gengamma did not converge and was removed.")
})
