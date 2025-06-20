test_that("silent and invisible", {
  expect_silent(get_version_citation(zenodo_id = "15643004",style = "apa",verbose = FALSE))
  expect_invisible(get_version_citation(zenodo_id = "15643004",style = "apa",verbose = TRUE))
})

test_that("citation is returned", {
  expect_snapshot(cat(get_version_citation(zenodo_id = "15643004",style = "apa",verbose = FALSE)))
})


test_that("errors when expected", {
  # unexpected value
  expect_error(get_version_citation(zenodo_id = "oldest",style = "apa",verbose = FALSE))
  # unlisted style
  expect_error(get_version_citation(zenodo_id = "15643004",style = "foo",verbose = FALSE))
  # multiple styles
  expect_error(get_version_citation(zenodo_id = "15643004",style = c("apa","ieee"),verbose = FALSE))
  # verbose is not logical
  expect_error(get_version_citation(zenodo_id = "15643004",style = "apa",verbose = NULL))
})
