test_that("successfully creates dictionary", {
  # get records
  download_deposit_version(zenodo_id = "15643004",dir_path = "outputs", datapackage_only = TRUE)
  dict <- get_data_dictionary("outputs/15643004/datapackage.json")

  # is a list
  expect_type(dict, "list")
  # has names
  expect_named(dict)

})

test_that("fails as expected", {
  # file doesnt exist
  expect_error(get_data_dictionary("fake/file.json"))

})
