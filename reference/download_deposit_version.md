# Download deposit version

Downloads and extracts some version of the deposit.

## Usage

``` r
download_deposit_version(
  zenodo_id,
  deposit_versions = list_deposit_versions(),
  dir_path,
  datapackage_only = FALSE
)
```

## Arguments

- zenodo_id:

  String. ID for a Zenodo deposit. Should correspond to the version of a
  deposit.

- deposit_versions:

  data frame. Output from `list_deposit_versions`

- dir_path:

  String. Path to directory where the files should be downloaded e.g.
  "inst/extdata/wdds_archive" note no trailing slash on the path.

- datapackage_only:

  Logical. Download only the datapackage.json file

## Value

String. Path to downloaded version.
