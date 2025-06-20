test_that("silent and invisible", {
  expect_silent(export_deposit_metadata(zenodo_id = "15643004",format = "json",verbose = FALSE))
  expect_invisible(export_deposit_metadata(zenodo_id = "15643004",format = "json",verbose = FALSE))
})

test_that("export is returned", {
  expect_snapshot(export_deposit_metadata(zenodo_id = "15643004",format = "bibtex",verbose = TRUE))
})


test_that("errors when expected", {
  # unexpected value
  expect_error(export_deposit_metadata(zenodo_id = "oldest",format = "json",verbose = TRUE))
  # unlisted format
  expect_error(export_deposit_metadata(zenodo_id = "15643004",format = "txt",verbose = TRUE))
  # multiple styles
  expect_error(export_deposit_metadata(zenodo_id = "15643004",format = c("json","bibtex"),verbose = TRUE))
  # verbose is not logical
  expect_error(export_deposit_metadata(zenodo_id = "15643004",format = "json",verbose = NULL))
})
