# Get Version Citation

Get Version Citation

## Usage

``` r
get_version_citation(
  zenodo_id = the$working_version,
  style = c("havard-cite-them-right", "apa", "modern-language-association", "vancouver",
    "chicago-fullnote-bibliography", "ieee"),
  verbose = TRUE
)
```

## Arguments

- zenodo_id:

  String. ID for a Zenodo deposit. Should correspond to the version of a
  deposit.

- style:

  Character. One of "havard-cite-them-right", "apa",
  "modern-language-association", "vancouver",
  "chicago-fullnote-bibliography", or "ieee"

- verbose:

  Logical. Print the citation?

## Value

Character. Text for a citation

## Examples

``` r

if (FALSE) { # \dontrun{
get_version_citation(zenodo_id = "15692263",
                    style = "apa")
} # }
```
