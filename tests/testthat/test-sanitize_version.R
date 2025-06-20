test_that("santizes version ids", {
  expect_equal(sanitize_version(" 15643003"), "15643003")
  expect_no_error(sanitize_version("latest"))
  # should convert numbers to character
  expect_no_error(sanitize_version(15643003))
})

test_that("errors when expected",{
  # unexpected value
  expect_error(sanitize_version("oldest"))
  # has letter in id
  expect_error(sanitize_version("123456OO"))
  # has decimal for some reason
  expect_error(sanitize_version(123.014))
  # has space for some reason
  expect_error(sanitize_version("123 014"))
})
