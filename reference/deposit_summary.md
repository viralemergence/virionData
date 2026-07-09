# Simple summary data frame for the deposit

Simple summary data frame for the deposit

## Usage

``` r
deposit_summary()
```

## Value

data frame. Simplified deposit metadata with the following fields:

- id = zenodo id

- latest_version = True or False indicating if that record is the latest
  version of the dataset.

- publication_date = YMD the item was published to zenodo

- doi_url = digital object identifier for that version of the record.

## Examples

``` r
deposit_summary()
#> data frame with 0 columns and 0 rows
```
