# Get all deposit versions

Recursively traverse a deposit link to get all versions of a deposit.

## Usage

``` r
get_all_versions(url)
```

## Arguments

- url:

  Character. URL for a version of the deposit.

## Value

Data.frame. Metadata for all deposit version

## Examples

``` r

if (FALSE) { # \dontrun{

self_link <- "https://zenodo.org/api/records/21232661/versions?page=1&size=25&sort=version"
get_all_versions(self_link)
} # }
```
