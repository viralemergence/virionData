test_that("errors when expected", {
  # zenodo id is an appropriate value
  expect_error(
    download_deposit_version(zenodo_id = "oldest",dir_path = "outputs", datapackage_only = TRUE)
    )

  # dir path is character
  expect_error(
    download_deposit_version(zenodo_id = "15643004",dir_path = 12356, datapackage_only = TRUE)
  )

  #datapackage only is logical
  expect_error(
    download_deposit_version(zenodo_id = "15643004",dir_path = "outputs", datapackage_only = NULL)
  )

})
