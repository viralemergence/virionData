# Get Data Dictionary for a data Package

Creates a list of data frames that constitutes the data dictionary.

## Usage

``` r
get_data_dictionary(datapackage_json)
```

## Arguments

- datapackage_json:

  Character. Path to datapackage.json file

## Value

List. Named list of data frames with the following fields:

- name = field name

- type = field type

- description = characterization of the field

## Examples

``` r

if (FALSE) { # \dontrun{
get_data_dictionary(datapackage_json = "data/1235600/datapackage.json")
} # }
```
