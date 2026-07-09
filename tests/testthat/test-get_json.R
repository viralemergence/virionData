test_that("get_json works", {
  url <- "https://zenodo.org/api/records/21232661/versions?page=1&size=25&sort=version"
  expect_no_error(get_json(url = url))
})

test_that("errors when expected",{
  # unexpected value
  expect_error(get_json(url = 1000))

  # unexpected value
  expect_error(get_json(url = "https://zenodo.org/api/recrods/fakerecord"))
})
