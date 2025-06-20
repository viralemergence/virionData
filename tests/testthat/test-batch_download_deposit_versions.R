test_that("errors when expected", {
  # inappropriate zenodo ids
  expect_error(batch_download_deposit_versions(zenodo_ids = "oldest",dir_path = "outputs", datapackage_only = TRUE))

  # non character dir
  expect_error(batch_download_deposit_versions(zenodo_ids = "oldest",dir_path = 123456, datapackage_only = TRUE))

  # inappropriate refresh
  expect_error(batch_download_deposit_versions(zenodo_ids = "oldest",
                                               dir_path = "outputs",
                                               refresh_deposits_versions = NULL,
                                               datapackage_only = TRUE))


})
