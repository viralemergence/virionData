# Make Zenodo URLs

Appends a zenodo id to the zenodo api url.

## Usage

``` r
make_url(base_url = "https://zenodo.org/api/records/%s", id)
```

## Arguments

- base_url:

  string. url for zenodo api

- id:

  string. zenodo id.

## Value

String. URL for zenodo api.

## Examples

``` r
make_url(id = "1235600")
#> [1] "https://zenodo.org/api/records/1235600"
```
