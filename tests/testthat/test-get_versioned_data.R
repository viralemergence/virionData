test_that("download works", {
  expect_no_error(get_versioned_data(dir_path = "outputs",datapackage_only = TRUE))
})

test_that("errors where expected", {

  # non-logical passed to data package only
  expect_error(get_versioned_data(dir_path = "outputs",datapackage_only = NULL))
  # non-character to dir_path
  expect_error(get_versioned_data(dir_path = 22222 ,datapackage_only = TRUE))
  # inappropriate version
  expect_error(get_versioned_data(version = "oldest",dir_path = "outputs" ,datapackage_only = TRUE))
  # inappropriate style
  expect_error(get_versioned_data(version = "latest",style = "foo", dir_path = "outputs" ,datapackage_only = TRUE))
  # inappropriate refresh_deposits_versions
  expect_error(get_versioned_data(version = "latest",style = "foo",refresh_deposits_versions = NULL, dir_path = "outputs" ,datapackage_only = TRUE))

  })
