test_that("invisible and list", {
  expect_invisible(list_deposit_versions(parent_id = "15643003"))
  expect_type(list_deposit_versions(parent_id = "15643003"),"list")
})

test_that("errors when expected", {
  # is character
  expect_error(list_deposit_versions(parent_id = 15643003))
  # non-id
  expect_error(list_deposit_versions(parent_id = "latest"))
})

