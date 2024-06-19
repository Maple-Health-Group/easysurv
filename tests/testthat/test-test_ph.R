test_that("test_ph function does not return an error", {
  expect_no_error(
    test_ph(
      data = easy_lung,
      time = "time",
      event = "status",
      group = "sex"
    )
  )
})

test_that("test_ph function without a group returns an error", {
  expect_error(
    test_ph(
      data = easy_lung,
      time = "time",
      event = "status"
    )
  )
})
