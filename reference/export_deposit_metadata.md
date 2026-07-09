# Export Deposit Metadata in various formats

Export Deposit Metadata in various formats

## Usage

``` r
export_deposit_metadata(
  zenodo_id = the$working_version,
  format,
  verbose = TRUE
)
```

## Arguments

- zenodo_id:

  String. ID for a Zenodo deposit. Should correspond to the version of a
  deposit.

- format:

  String. File format for export. One of "json",
  "json-ld","csl","datacite-json","datacite-xml",
  "dublincore","marcxml","bibtex","geojson","dcat-ap","codemeta", or
  "cff"

- verbose:

  Logical. Print the formatted metadata?

## Value

Character. Returns text in selected format.

## Examples

``` r
 if (FALSE) { # \dontrun{
export_deposit_metadata(zenodo_id = "15692263","json-ld" ) |>
  writeLines(con = "outputs/citation.jsonld")
} # }
```
