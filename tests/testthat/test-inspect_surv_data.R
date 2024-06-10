test_that("inspect_surv_data() does not return an error", {
  expect_no_error(
    inspect_surv_data(
      data = easy_lung,
      time = "time",
      event = "status",
      group = "sex"
    )
  )
})

test_that("inspect_surv_data() does not return an error without group", {
  expect_no_error(
    inspect_surv_data(
      data = easy_lung,
      time = "time",
      event = "status"
    )
  )
})
