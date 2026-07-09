# Check version ids

Checks that version ids are properly formatted. IDs should either be
integers ("15643003") OR "latest"

## Usage

``` r
sanitize_version(version)
```

## Arguments

- version:

  Character. Version identifier.

## Value

Character. Version identifier from Zenodo.

## Examples

``` r

sanitize_version("latest")
#> [1] "21232661"
sanitize_version(" 15643003")
#> [1] "15643003"
```
