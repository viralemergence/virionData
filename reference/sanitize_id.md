# Sanitize a zenodo id

Remove any white space and check that it conforms to the zenodo id
pattern.

## Usage

``` r
sanitize_id(zenodo_id)
```

## Arguments

- zenodo_id:

  String. A zenodo id.

## Value

String. A cleaned zenodo id.

## Examples

``` r
sanitize_id(" 2948598")
#> [1] "2948598"
```
