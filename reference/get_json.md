# Get json

[`jsonlite::fromJSON()`](https://jeroen.r-universe.dev/jsonlite/reference/fromJSON.html)
wrapped in
[`rlang::try_fetch()`](https://rlang.r-lib.org/reference/try_fetch.html)
to make calls smoother.

## Usage

``` r
get_json(url)
```

## Arguments

- url:

  String. A url fed to
  [`jsonlite::fromJSON()`](https://jeroen.r-universe.dev/jsonlite/reference/fromJSON.html)

## Value

List. JSON as a list

## Examples

``` r
if (FALSE) { # \dontrun{
get_json("https://www.example.com/example.json")
} # }
```
