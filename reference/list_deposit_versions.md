# Get Versions of a Deposit on Zenodo

This function gets the metadata for all the versions of a deposit
associated with a parent id. The parent id is used to identify a set of
works that are different versions of the same work. The parent id is
provided by the Zenodo API. To find the parent ID, download a JSON
representation of the deposit (export to json on the webpage or use
`export_deposit_metadata`), there will be an attribute called parent
that looks like "https://zenodo.org/api/records/15020049". The 8 digit
string at the end of the url is the parent id.

## Usage

``` r
list_deposit_versions(parent_id = "15643003")
```

## Arguments

- parent_id:

  String. Identifier for a Zenodo deposit with multiple versions.

## Value

Data frame. Invisible. The data frame contains the Zenodo id for each
version of the deposit, as well as the version name, and logical field
called latest that indicates if this is the latest version.

## Details

This function also sets package variables
