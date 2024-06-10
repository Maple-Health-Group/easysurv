test_that("get_km function does not return an error", {
  expect_no_error(
    get_km(
      data = easy_lung,
      time = "time",
      event = "status",
      group = "sex"
    )
  )
})

test_that("get_km function without a group does not return an error", {
  expect_no_error(
    get_km(
      data = easy_lung,
      time = "time",
      event = "status"
    )
  )
})
