# Get bibtex entry for a deposit.

Bibtex specific wrapper for export_deposit_metadata

## Usage

``` r
export_deposit_bibtex(zenodo_id = the$working_version, verbose = TRUE)
```

## Arguments

- zenodo_id:

  String. ID for a Zenodo deposit. Should correspond to the version of a
  deposit.

- verbose:

  Logical. Print the formatted metadata?

## Value

Character. bibtex formatted entry

## Examples

``` r
if (FALSE) { # \dontrun{
export_deposit_bibtex(zenodo_id = "15692263" ) |>
  writeLines(con = "outputs/citation.bib")
} # }
```
