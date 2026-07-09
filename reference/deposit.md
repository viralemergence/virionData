# deposit

An R6 client for getting virion data and metadata.

## Value

A `deposit` class (R6 class)

invisibly returns self

dataframe of remote csv file.

String. Path to download location.

data.frame from
[`readr::read_csv()`](https://readr.tidyverse.org/reference/read_delim.html)

String. cleaned zenodo id

metadata in the specificed format (invisible)

List. Named list of data frames with the following fields:

- name = field name

- type = field type

- description = characterization of the field

parent_id String. Identifier for a Zenodo deposit with multiple
versions.

all_versions Character. All zenodo ids for versions of the deposit.

summary_df Data frame. A table of all zenodo ids for versions of the
deposit.

latest_version String. Identifier for the latest version of the zenodo
deposit.

working_version String. Identifier for the version of the zenodo deposit
you are working with

working_bibtex String. Bibtex for the version of the zenodo deposit you
are working with.

working_citation String. Citation for the version of the zenodo deposit
you are working with.

working_files Data frame. Working files consist of key (file name) and
url (url to file on zenodo)

working_json list. Metadata for the working version.

## Methods

### Public methods

- [`deposit$new()`](#method-deposit-new)

- [`deposit$set_working_version()`](#method-deposit-set_working_version)

- [`deposit$load_remote_csv_file()`](#method-deposit-load_remote_csv_file)

- [`deposit$download_versioned_data()`](#method-deposit-download_versioned_data)

- [`deposit$load_local_csv_file()`](#method-deposit-load_local_csv_file)

- [`deposit$check_zenodo_id()`](#method-deposit-check_zenodo_id)

- [`deposit$export_metadata()`](#method-deposit-export_metadata)

- [`deposit$get_data_dictionary()`](#method-deposit-get_data_dictionary)

- [`deposit$get_parent_id()`](#method-deposit-get_parent_id)

- [`deposit$get_all_versions()`](#method-deposit-get_all_versions)

- [`deposit$get_summary_df()`](#method-deposit-get_summary_df)

- [`deposit$get_latest_version()`](#method-deposit-get_latest_version)

- [`deposit$get_working_version()`](#method-deposit-get_working_version)

- [`deposit$get_working_bibtex()`](#method-deposit-get_working_bibtex)

- [`deposit$get_working_citation()`](#method-deposit-get_working_citation)

- [`deposit$get_working_files()`](#method-deposit-get_working_files)

- [`deposit$get_working_json()`](#method-deposit-get_working_json)

- [`deposit$clone()`](#method-deposit-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new `deposit` object, as an
[R6](https://r6.r-lib.org/reference/R6-package.html) client. This object
is connected to the zenodo deposit for virion and is the gateway for
accessing different versions of the data.

#### Usage

    deposit$new(parent_id = "15643003")

#### Arguments

- `parent_id`:

  String. Identifier for a Zenodo deposit with multiple versions.

#### Returns

A new `deposit` object

#### Examples

    \dontrun{
    virion_deposit <- deposit$new()
    }

------------------------------------------------------------------------

### Method `set_working_version()`

Set the version of the data you would like to work with. Note that the
structure of Virion data may change through time.

#### Usage

    deposit$set_working_version(
      zenodo_id = "latest",
      style = "apa",
      print_bibtex = TRUE,
      print_citation = TRUE
    )

#### Arguments

- `zenodo_id`:

  String. A zenodo id or latest.

- `style`:

  String. The citation style to be used.

- `print_bibtex`:

  Logical. Should the bibtex for the working version of the deposit be
  printed when setting the working version?

- `print_citation`:

  Logical. Should the citation for the working version of the deposit be
  printed?

#### Examples


    \dontrun{
    # set the working version to the latest version of the deposit
    virion_deposit$set_working_version("latest")

    # set the working versino to an arbitrary deposit
    virion_deposit$get_all_versions()
    virion_deposit$set_working_version("19502921")
    }

------------------------------------------------------------------------

### Method `load_remote_csv_file()`

Load a remote csv listed in the working files field directly into the
environment.

Can handle either csv or csv.gz files. This function downloads a
temporary file to your machine, reads it, then returns a dataframe.

The local file cache is removed when your R session restarts.

Consider using the `download_versioned_data` to save a persistent
version of the virion dataset on your machine.

#### Usage

    deposit$load_remote_csv_file(file_key, refresh = FALSE, ...)

#### Arguments

- `file_key`:

  String. A file key as seen in the `working_files` field

- `refresh`:

  Logical. Should files be re-downloaded.

- `...`:

  Additional arguments to pass to
  [`readr::read_csv()`](https://readr.tidyverse.org/reference/read_delim.html)

#### Examples

    \dontrun{
    ## see the working files
    working_files <- virion$get_working_files()
    working_files$file_key
    virion_data <- virion_deposit$load_remote_csv_file("virion.csv.gz")

    }

------------------------------------------------------------------------

### Method `download_versioned_data()`

Download versioned data

Downloads all the files for a particular version of Virion to
`dir/zenodo_id`.

This function does not change the working version!

#### Usage

    deposit$download_versioned_data(
      zenodo_id = "working",
      dir = "outputs",
      refresh = FALSE
    )

#### Arguments

- `zenodo_id`:

  String. A zenodo id, working or latest

- `dir`:

  String. Storage location for files

- `refresh`:

  Logical. Should files be re-downloaded.

#### Examples

    \dontrun{
    # download files for the working version
    virion_deposit$download_versioned_data()

    # download files for the latest version
    virion_deposit$download_versioned_data("latest")

    # download files for some zenodo id
    virion_deposit$download_versioned_data("19502921")
    }

------------------------------------------------------------------------

### Method `load_local_csv_file()`

Load a local version of the csv.

Assumes you would like to load the file from the current working
version.

If the csv file is not present, a persistent version of the file will be
downloaded to your machine.

#### Usage

    deposit$load_local_csv_file(file_key, refresh, ...)

#### Arguments

- `file_key`:

  string. Name of file from `working_files`

- `refresh`:

  logical. Should the file be re-downloaded?

- `...`:

  Additional parameters to pass to
  [`readr::read_csv()`](https://readr.tidyverse.org/reference/read_delim.html)

#### Examples

    \dontrun{

    virion <- deposit$new()
    virion$set_working_version()
    working_files <- virion$get_working_files()
    working_files$file_key
    virion$load_local_csv_file("virion.csv.gz")
    }

------------------------------------------------------------------------

### Method `check_zenodo_id()`

Check the zenodo ID

Look for key words like working or latest and convert them to integer
ids OR pass the zenodo id directly to
[`sanitize_id()`](https://viralemergence.github.io/virionData/reference/sanitize_id.md)

#### Usage

    deposit$check_zenodo_id(zenodo_id)

#### Arguments

- `zenodo_id`:

  String. One of working, latest, or a zenodo id

#### Examples


    \dontrun{
    virion_deposit$check_zenodo_id("latest")
    }

------------------------------------------------------------------------

### Method `export_metadata()`

Export metadata for a version of the deposit.

#### Usage

    deposit$export_metadata(zenodo_id, format, verbose)

#### Arguments

- `zenodo_id`:

  String. One of "latest","working", or a zenodo id

- `format`:

  String. One of "json", "json-ld","csl","datacite-json","datacite-xml",
  "dublincore","marcxml","bibtex","geojson","dcat-ap","codemeta", "cff"

- `verbose`:

  Logical. Print the metadata

#### Examples


    \dontrun{
     virion_deposit$export_metadata("working")
    }

------------------------------------------------------------------------

### Method [`get_data_dictionary()`](https://viralemergence.github.io/virionData/reference/get_data_dictionary.md)

Create a list of data dictionaries.

#### Usage

    deposit$get_data_dictionary(
      file_key = "datapackage.json",
      dir = tempdir(),
      refresh
    )

#### Arguments

- `file_key`:

  string. Name of file from `working_files`

- `dir`:

  string. Name of directory

- `refresh`:

  refresh Logical. Should files be re-downloaded.

#### Examples

    \dontrun{
    virion <- deposit$new()
    virion$set_working_version()
    virion$get_data_dictionary()

    }

------------------------------------------------------------------------

### Method `get_parent_id()`

Get a property of the object that is private

- all_versions Character. All zenodo ids for versions of the deposit.

- summary_df Data frame. A table of all zenodo ids for versions of the
  deposit.

- latest_version String. Identifier for the latest version of the zenodo
  deposit.

- working_version String. Identifier for the version of the zenodo
  deposit you are working with.

- working_bibtex String. Bibtex for the version of the zenodo deposit
  you are working with.,

- working_citation String. Citation for the version of the zenodo
  deposit you are working with.

- working_files Data frame. Working files consist of key (file name) and
  url (url to file on zenodo)

- working_json list. Metadata for the working version.

#### Usage

    deposit$get_parent_id()

------------------------------------------------------------------------

### Method [`get_all_versions()`](https://viralemergence.github.io/virionData/reference/get_all_versions.md)

Get a property of the object that is private

#### Usage

    deposit$get_all_versions()

------------------------------------------------------------------------

### Method `get_summary_df()`

Get a property of the object that is private

- latest_version String. Identifier for the latest version of the zenodo
  deposit.

- working_version String. Identifier for the version of the zenodo
  deposit you are working with.

- working_bibtex String. Bibtex for the version of the zenodo deposit
  you are working with.,

- working_citation String. Citation for the version of the zenodo
  deposit you are working with.

- working_files Data frame. Working files consist of key (file name) and
  url (url to file on zenodo)

- working_json list. Metadata for the working version.

#### Usage

    deposit$get_summary_df()

------------------------------------------------------------------------

### Method `get_latest_version()`

Get a property of the object that is private

#### Usage

    deposit$get_latest_version()

------------------------------------------------------------------------

### Method `get_working_version()`

Get a property of the object that is private

#### Usage

    deposit$get_working_version()

------------------------------------------------------------------------

### Method `get_working_bibtex()`

Get a property of the object that is private

#### Usage

    deposit$get_working_bibtex()

------------------------------------------------------------------------

### Method `get_working_citation()`

Get a property of the object that is private

#### Usage

    deposit$get_working_citation()

------------------------------------------------------------------------

### Method `get_working_files()`

Get a property of the object that is private

#### Usage

    deposit$get_working_files()

------------------------------------------------------------------------

### Method `get_working_json()`

Get a property of the object that is private

#### Usage

    deposit$get_working_json()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    deposit$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
# make a client
virion_deposit <- deposit$new()

# methods
virion_deposit$set_working_version("latest")
virion_deposit$get_working_files()

# load remote data directly into your session
virion_data  <- virion_deposit$load_remote_csv_file(file_key = "virion.csv.gz")

# download remote data to disk - defaults to working version
virion_deposit$download_versioned_data()

} # }

## ------------------------------------------------
## Method `deposit$new`
## ------------------------------------------------

if (FALSE) { # \dontrun{
virion_deposit <- deposit$new()
} # }

## ------------------------------------------------
## Method `deposit$set_working_version`
## ------------------------------------------------


if (FALSE) { # \dontrun{
# set the working version to the latest version of the deposit
virion_deposit$set_working_version("latest")

# set the working versino to an arbitrary deposit
virion_deposit$get_all_versions()
virion_deposit$set_working_version("19502921")
} # }


## ------------------------------------------------
## Method `deposit$load_remote_csv_file`
## ------------------------------------------------

if (FALSE) { # \dontrun{
## see the working files
working_files <- virion$get_working_files()
working_files$file_key
virion_data <- virion_deposit$load_remote_csv_file("virion.csv.gz")

} # }

## ------------------------------------------------
## Method `deposit$download_versioned_data`
## ------------------------------------------------

if (FALSE) { # \dontrun{
# download files for the working version
virion_deposit$download_versioned_data()

# download files for the latest version
virion_deposit$download_versioned_data("latest")

# download files for some zenodo id
virion_deposit$download_versioned_data("19502921")
} # }


## ------------------------------------------------
## Method `deposit$load_local_csv_file`
## ------------------------------------------------

if (FALSE) { # \dontrun{

virion <- deposit$new()
virion$set_working_version()
working_files <- virion$get_working_files()
working_files$file_key
virion$load_local_csv_file("virion.csv.gz")
} # }


## ------------------------------------------------
## Method `deposit$check_zenodo_id`
## ------------------------------------------------


if (FALSE) { # \dontrun{
virion_deposit$check_zenodo_id("latest")
} # }

## ------------------------------------------------
## Method `deposit$export_metadata`
## ------------------------------------------------


if (FALSE) { # \dontrun{
 virion_deposit$export_metadata("working")
} # }


## ------------------------------------------------
## Method `deposit$get_data_dictionary`
## ------------------------------------------------

if (FALSE) { # \dontrun{
virion <- deposit$new()
virion$set_working_version()
virion$get_data_dictionary()

} # }
```
