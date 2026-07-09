test_that("download_refresh works", {
  expect_no_error(
    download_refresh(file_url = "https://zenodo.org/api/records/21232661/files/tax_table.csv.gz/content",
                     local = tempfile(fileext = "csv.gz"),
                     refresh = FALSE
                     )
    )
})

test_that("errors when expected",{
  # unexpected value
  expect_error(
    download_refresh(file_url = "https://zenodo.org/api/records/21232661/files/tax_table.csv.gz/content",
                     local = tempfile(fileext = "csv.gz"),
                     refresh = 555
    )
  )

})
