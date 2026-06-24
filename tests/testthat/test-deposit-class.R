test_that("init works", {
  expect_no_error(virion_deposit <- deposit$new())
})


test_that("set working version works", {
  virion_deposit <- deposit$new()
  expect_no_error(virion_deposit$set_working_version("latest"))
})

test_that("set working version works", {
  virion_deposit <- deposit$new()
  expect_error(virion_deposit$set_working_version(FALSE))
})


test_that("load remote work", {
  virion_deposit <- deposit$new()
  virion_deposit$set_working_version("latest")
  expect_no_error(
  virion_deposit$load_remote_csv_file(file_key = "db_table.csv")
  )
})


test_that("download remote works", {
  virion_deposit <- deposit$new()
  virion_deposit$set_working_version("latest")
  expect_no_error(
    virion_deposit$download_versioned_data(dir = tempdir())
  )
})

test_that("load local works", {
  virion_deposit <- deposit$new()
  virion_deposit$set_working_version("latest")
  virion_deposit$download_versioned_data(dir = tempdir())
    expect_no_error(
  virion_deposit$load_local_csv_file(file_key = "db_table.csv",refresh = FALSE)
  )
})


test_that("get data dictionary", {
  virion_deposit <- deposit$new()
  virion_deposit$set_working_version("latest")
  expect_no_error(
    virion_deposit$get_data_dictionary(refresh = FALSE)
  )
})



test_that("load local works", {
  virion_deposit <- deposit$new()
  virion_deposit$set_working_version("latest")
  virion_deposit$download_versioned_data(dir = tempdir())
  expect_no_error(
    virion_deposit$load_local_csv_file(file_key = "db_table.csv",refresh = FALSE)
  )
})
