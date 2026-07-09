test_that("make_url works", {
  expect_no_error(make_url(id = "100030"))
})

test_that("errors when expected",{
  # unexpected value
  expect_error(make_url(base_url = 299))

  # unexpected value
  expect_error(make_url(id = FALSE))
})
