# Batch download deposit versions

This is `download_deposit_version` wrapped in a `purr::map` call.

## Usage

``` r
batch_download_deposit_versions(zenodo_ids = "all", dir_path, ...)
```

## Arguments

- zenodo_ids:

  Character. Either a vector of zenodo ids or "all"

- dir_path:

  Character. Path to folder where files should be downloaded.

- ...:

  Other arguments passed to `download_deposit_version`

## Value

List of download locations.

## Examples

``` r
if (FALSE) { # \dontrun{
# get all deposit versions
batch_download_deposit_versions(dir_path = "outputs")

# get select versions
batch_download_deposit_versions(zenodo_ids = c("15677137", "15643004"), dir_path = "outputs")
} # }
```
