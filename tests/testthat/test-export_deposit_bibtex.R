test_that("silent and invisible", {
  expect_silent(export_deposit_bibtex(zenodo_id = "15643004",verbose = FALSE))
  expect_invisible(export_deposit_bibtex(zenodo_id = "15643004",verbose = FALSE))
})

test_that("export is returned", {
  expect_snapshot(export_deposit_bibtex(zenodo_id = "15643004",verbose = TRUE))
})


test_that("errors when expected", {
  # unexpected value
  expect_error(export_deposit_bibtex(zenodo_id = "oldest",verbose = TRUE))
  # verbose is not logical
  expect_error(export_deposit_bibtex(zenodo_id = "15643004",verbose = NULL))
})

