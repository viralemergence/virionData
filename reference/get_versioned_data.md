# Get a specific version of the data

This is a convenience function that downloads a specific version of the
data and returns attribution information.

## Usage

``` r
get_versioned_data(
  version = "latest",
  style = "apa",
  dir_path,
  refresh_deposits_versions = TRUE,
  verbose = TRUE,
  ...
)
```

## Arguments

- version:

  Character. identifier for a version e.g. "15643003" or "latest"

- style:

  Charater. Character. One of "havard-cite-them-right", "apa",
  "modern-language-association", "vancouver",
  "chicago-fullnote-bibliography", or "ieee"

- dir_path:

  Character. Path to folder where files should be downloaded.

- refresh_deposits_versions:

  Logical. Should the function check for new versions of the deposit?

- verbose:

  Logical. Include print statements?

- ...:

  Additional arguments to pass to `download_deposit_version`

## Value

Character. path to versioned data.

## Details

If you would like to download a bibtex entry use
`export_deposit_bibtex`. For other metadata types (CSL, CFF, JSON-LD,
etcetera) use `export_deposit_metadata`.
