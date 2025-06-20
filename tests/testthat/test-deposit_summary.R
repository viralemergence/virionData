test_that("returns a list type object", {
  expect_type(deposit_summary(), "list")
})

test_that("runs sucessfully", {
  expect_no_error(deposit_summary())
})
