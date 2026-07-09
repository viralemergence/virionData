# Getting Started

``` r

library(virionData)
library(frictionless)
library(kableExtra)
library(glue)
library(stringr)
```

The **virionData** package is meant to facilitate access to the virion
dataset on Zenodo and nudge users towards best practices for data use.

## Make a new deposit object

The virionData package contains a [class of
object](https://r6.r-lib.org/articles/Introduction.html#basics) called a
deposit. The deposit object contains attributes related to the Zenodo
deposit, allows you to establish a working version of the data, and
contains methods for accessing versioned data.

``` r


virion <- deposit$new()
# see the latest version of virion 
virion$get_latest_version()
#> [1] "21232661"
# see a table of all versions, their publication dates, and doi's
virion$get_summary_df() |> head()
#>         id latest_version publication_date
#> 1 21232661           TRUE       2026-07-07
#> 2 21056787          FALSE       2026-06-30
#> 3 20818239          FALSE       2026-06-23
#> 4 20712496          FALSE       2026-06-16
#> 5 20618632          FALSE       2026-06-09
#> 6 20604548          FALSE       2026-06-09
#>                                   doi_url
#> 1 https://doi.org/10.5281/zenodo.21232661
#> 2 https://doi.org/10.5281/zenodo.21056787
#> 3 https://doi.org/10.5281/zenodo.20818239
#> 4 https://doi.org/10.5281/zenodo.20712496
#> 5 https://doi.org/10.5281/zenodo.20618632
#> 6 https://doi.org/10.5281/zenodo.20604548
```

## Set the working version

After you make a new deposit object, you will likely want to set the
data version. Here you can supply a specific Zenodo id OR simply use
“latest” to get the latest version of the data.

Setting the working version will add some additional metadata to your
virion object and enable you to start working with data.

`virion$working_files` will be added to the object, allowing you to
specify the files you’d like to work with.

``` r


# get the latest version of the data
virion$set_working_version(zenodo_id = "latest")
#> @misc{collin_schwantes_2026_21232661,
#>   author       = {Collin Schwantes and
#>                   Cole Brookson and
#>                   Timothée Poisot and
#>                   Tad Dallas and
#>                   Greg Albery and
#>                   Colin J. Carlson and
#>                   Cecilia A. Sanchez and
#>                   Renata Muylaert and
#>                   Evan Eskew and
#>                   Rory Gibb and
#>                   Maxwell J Farrell},
#>   title        = {The Global Virome in One Network (VIRION): Data
#>                    Package
#>                   },
#>   month        = jul,
#>   year         = 2026,
#>   publisher    = {Zenodo},
#>   doi          = {10.5281/zenodo.21232661},
#>   url          = {https://doi.org/10.5281/zenodo.21232661},
#> }
#> Collin Schwantes, Cole Brookson, Timothée Poisot, Tad Dallas, Greg Albery, Colin J. Carlson, Cecilia A. Sanchez, Renata Muylaert, Evan Eskew, Rory Gibb& Maxwell J Farrell. (2026). The Global Virome in One Network (VIRION): Data Package. Zenodo. https://doi.org/10.5281/zenodo.21232661
# Get a specific version of the data. 
# It is usually a good idea to explicitly provide the version id. 
virion$set_working_version(zenodo_id = "20517628", print_bibtex = FALSE,
                           print_citation = FALSE)

# after you've set the working version, you can get the working citation or working bibtex.
virion$get_working_citation()
#> [1] "Collin Schwantes, Cole Brookson, Timothée Poisot, Tad Dallas, Greg Albery, Colin J. Carlson, Cecilia A. Sanchez, Renata Muylaert, Evan Eskew, Rory Gibb& Maxwell J Farrell. (2026). The Global Virome in One Network (VIRION): Data Package. Zenodo. https://doi.org/10.5281/zenodo.20517628"

# you can also see the files contained in that version of virion
# the file_key field is used throughout the package to access the files.
virion$get_working_files()
#>                 file_key
#> 1           db_table.csv
#> 2       detection.csv.gz
#> 3      taxonomy_host.csv
#> 4     taxonomy_virus.csv
#> 5           edgelist.csv
#> 6        temporal.csv.gz
#> 7      provenance.csv.gz
#> 8  ncbi_accession.csv.gz
#> 9       tax_table.csv.gz
#> 10         virion.csv.gz
#> 11      datapackage.json
#>                                                                       file_url
#> 1           https://zenodo.org/api/records/20517628/files/db_table.csv/content
#> 2       https://zenodo.org/api/records/20517628/files/detection.csv.gz/content
#> 3      https://zenodo.org/api/records/20517628/files/taxonomy_host.csv/content
#> 4     https://zenodo.org/api/records/20517628/files/taxonomy_virus.csv/content
#> 5           https://zenodo.org/api/records/20517628/files/edgelist.csv/content
#> 6        https://zenodo.org/api/records/20517628/files/temporal.csv.gz/content
#> 7      https://zenodo.org/api/records/20517628/files/provenance.csv.gz/content
#> 8  https://zenodo.org/api/records/20517628/files/ncbi_accession.csv.gz/content
#> 9       https://zenodo.org/api/records/20517628/files/tax_table.csv.gz/content
#> 10         https://zenodo.org/api/records/20517628/files/virion.csv.gz/content
#> 11      https://zenodo.org/api/records/20517628/files/datapackage.json/content
```

## Start working with data

Here you have two options. One is to download a persistent copy of the
data to your machine and the other is to load the data from Zenodo
without storing a persistent copy.

*Note* Starting in May 2026, taxonomic data is split from the main
`virion.csv.gz` file and can be joined based on TaxHashID.

`load_*_csv` assumes you are trying to load the data from the current
working version.

``` r


# if the csv is not already downloaded, it will be downloaded first
virion_data <- virion$load_local_csv_file(file_key = "virion.csv.gz",
                                          refresh = TRUE)
#> refreshing file cache
#> refreshing file cache
#> refreshing file cache
#> refreshing file cache
#> refreshing file cache
#> refreshing file cache
#> refreshing file cache
#> refreshing file cache
#> refreshing file cache
#> refreshing file cache
#> refreshing file cache
#> Rows: 883890 Columns: 20
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr  (9): HostOriginal, VirusOriginal, DetectionMethod, DetectionOriginal, D...
#> dbl  (9): PublicationYear, PMID, ReleaseYear, ReleaseMonth, ReleaseDay, Coll...
#> lgl  (1): HostFlagID
#> date (1): Release_Date
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

# loading a remote csv creates a temporary file that will be deleted when
# the R session ends.
virion_data_remote <- virion$load_remote_csv_file(file_key = "virion.csv.gz")
#> downloading file
#> Rows: 883890 Columns: 20
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr  (9): HostOriginal, VirusOriginal, DetectionMethod, DetectionOriginal, D...
#> dbl  (9): PublicationYear, PMID, ReleaseYear, ReleaseMonth, ReleaseDay, Coll...
#> lgl  (1): HostFlagID
#> date (1): Release_Date
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

# While files for a particular version of virion on Zenodo are unlikely to change,
# you can re-download cached files by using the the refresh parameter. 
virion_data_remote <- virion$load_remote_csv_file(file_key = "virion.csv.gz",
                                                  refresh = TRUE)
#> refreshing file cache
#> Rows: 883890 Columns: 20
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr  (9): HostOriginal, VirusOriginal, DetectionMethod, DetectionOriginal, D...
#> dbl  (9): PublicationYear, PMID, ReleaseYear, ReleaseMonth, ReleaseDay, Coll...
#> lgl  (1): HostFlagID
#> date (1): Release_Date
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

## What do the fields mean?

The `datapackage.json` describes the contents of the files, including
field level descriptions. The `get_data_dictionary` method creates a
table of field names, types, and descriptions for each csv file.

``` r


data_dictionary <- virion$get_data_dictionary(refresh = FALSE)
#> downloading file

data_dictionary$virion_csv |>
  dplyr::mutate(description = stringr::str_wrap(description)) |>
  kableExtra::kbl() |>
  kableExtra::kable_material(full_width = FALSE) |>
  kableExtra::column_spec(3, width = "30em") 
```

| name | type | description |
|:---|:---|:---|
| HostOriginal | string | Host name from original dataset |
| VirusOriginal | string | Virus name from original dataset |
| HostFlagID | boolean | Denotes the presence of possible uncertainty in host identification, which users may want to check before proceeding any further. |
| DetectionMethod | string | Four harmonized categories in descending order of strength of evidence: “Isolation/Observation,” “PCR/Sequencing,” “Antibodies,” and “Not specified”. In some cases where detection method is not available via metadata, source information is used as DetectionOriginal (e.g., “NCBI Nucleotide”). |
| DetectionOriginal | string | Method used for determing the presence of a virus as described in the original work |
| DatabaseVersion | string | For static data, a citation. For dynamic data (e.g. Genbank) the access URL and a time stamp |
| PublicationYear | number | For literature derived records. PublicationYear provides the year the literature source was published, accessed either from the original database’s reference description or from scraping the PubMed database. |
| ReferenceText | string | A text description of literature sources |
| PMID | number | PubMed identifiers for literature sources |
| ReleaseYear | number | The year a given association was “released” in public information (EID2 and PREDICT) or a publicly deposited sample on GenBank. For PREDICT, all values are given as 2021, given the release of a static file at that time even though some findings may have been published or deposited in GenBank earlier. (This redundancy should be captured in overlap with GenBank and EID2.) |
| ReleaseMonth | number | The month a given association was “released” to the public |
| ReleaseDay | number | The day a given association was “released” to the public |
| CollectionYear | number | Reports the year of actual sample collection (GenBank and Predict) |
| CollectionMonth | number | Reports the month of actual sample collection (GenBank and Predict) |
| CollectionDay | number | Reports the day of actual sample collection (GenBank and Predict) |
| HostTaxHashID | string | Foreign Key for the taxa definitions table |
| VirusTaxHashID | string | Foreign Key for the taxa definitions table |
| AssocID | number | Row number used as an id for a host-virus association. Will be specific to a given version of the data |
| Release_Date | date | Date data were released |
| Collection_Date | string | Date of actual sample collection |

## Citation information

You can get the citation text or bibtex for the working version.

``` r


virion$get_working_citation() |>
  stringr::str_wrap() |>
  cat()
#> Collin Schwantes, Cole Brookson, Timothée Poisot, Tad Dallas, Greg Albery, Colin
#> J. Carlson, Cecilia A. Sanchez, Renata Muylaert, Evan Eskew, Rory Gibb& Maxwell
#> J Farrell. (2026). The Global Virome in One Network (VIRION): Data Package.
#> Zenodo. https://doi.org/10.5281/zenodo.20517628
```

``` r

virion$get_working_bibtex() |> cat()
#> @misc{collin_schwantes_2026_20517628,
#>   author       = {Collin Schwantes and
#>                   Cole Brookson and
#>                   Timothée Poisot and
#>                   Tad Dallas and
#>                   Greg Albery and
#>                   Colin J. Carlson and
#>                   Cecilia A. Sanchez and
#>                   Renata Muylaert and
#>                   Evan Eskew and
#>                   Rory Gibb and
#>                   Maxwell J Farrell},
#>   title        = {The Global Virome in One Network (VIRION): Data
#>                    Package
#>                   },
#>   month        = jun,
#>   year         = 2026,
#>   publisher    = {Zenodo},
#>   doi          = {10.5281/zenodo.20517628},
#>   url          = {https://doi.org/10.5281/zenodo.20517628},
#> }
```

## Working version deposit metadata

If you want to access the full deposit metadata for the current working
version, maybe to dig into the persistent identifier graph or pull out
the code version used to make the data, you can do that by looking at
the `working_json` field.

``` r

## get the working json
working_json <- virion$get_working_json()

## When was the version last modified?
working_json$modified
#> [1] "2026-06-02T22:19:54.305277+00:00"

## What version of the `virion` codebase was used to make this version of virion?
working_json$metadata$related_identifiers |>
  dplyr::filter(resource_type == "software") |>
  dplyr::pull(identifier) |>
  (\(val) glue::glue("https://doi.org/{val}"))()
#> https://doi.org/10.5281/zenodo.20517323
```

## Functional programming approach

This approach uses package environment variables, making working with
multiple versions slightly more complicated. Users also need to be aware
of when the working version will and won’t be changed, and when that
will impact the way functions work.

Changes set in the R6 object will not be propogated

### Check available versions of the dataset

``` r

 list_deposit_versions()
  deposit_summary() |>
    head()
#>         id latest_version publication_date
#> 1 21232661           TRUE       2026-07-07
#> 2 21056787          FALSE       2026-06-30
#> 3 20818239          FALSE       2026-06-23
#> 4 20712496          FALSE       2026-06-16
#> 5 20618632          FALSE       2026-06-09
#> 6 20604548          FALSE       2026-06-09
#>                                   doi_url
#> 1 https://doi.org/10.5281/zenodo.21232661
#> 2 https://doi.org/10.5281/zenodo.21056787
#> 3 https://doi.org/10.5281/zenodo.20818239
#> 4 https://doi.org/10.5281/zenodo.20712496
#> 5 https://doi.org/10.5281/zenodo.20618632
#> 6 https://doi.org/10.5281/zenodo.20604548
```

## Get latest version of the data

``` r

# get data - by default this is the latest version of the data.
get_versioned_data(version = "15692263", dir_path = "outputs")
#> Cole Brookson, Collin Schwantes, Timothée Poisot, Tad Dallas, Greg Albery, Colin J. Carlson, Cecilia A. Sanchez, Renata Muylaert, Evan Eskew, Rory Gibb& Maxwell J Farrell. (2025). The Global Virome in One Network (VIRION): Data Package [Dataset]. Zenodo. https://doi.org/10.5281/zenodo.15692263
#> deposit will download to outputs/15692263
#> outputs/15692263
# list files
fs::dir_ls("outputs/15692263")
#> outputs/15692263/datapackage.json   outputs/15692263/detection.csv.gz   
#> outputs/15692263/edgelist.csv       outputs/15692263/provenance.csv.gz  
#> outputs/15692263/taxonomy_host.csv  outputs/15692263/taxonomy_virus.csv 
#> outputs/15692263/temporal.csv.gz    outputs/15692263/virion.csv.gz
```

## Read the data

Now that you have the data locally, you can read it! The virion files
are comma delimited with period decimal markers.

``` r


virion <- vroom::vroom(file = "outputs/15692263/virion.csv.gz")
```

## Cite the data

Citing data makes increases reproducibility and incentivizes data
sharing.

``` r


# by default the citation will be generated for the current working version.
# this is set when we run `get_versioned_data`
get_version_citation(style = "modern-language-association")
#> Cole Brookson, et al. “The Global Virome in One Network (VIRION): Data
#> Package”. Zenodo, 18 June 2025, https://doi.org/10.5281/zenodo.15692263.
```

``` r

# we can cite a specific version by providing a zenodo id
get_version_citation(zenodo_id = "15643004",style = "apa")
#> Cole Brookson, Collin Schwantes, Timothée Poisot, Tad Dallas, Greg Albery,
#> Colin J. Carlson, Cecilia A. Sanchez, Renata Muylaert, Evan Eskew, Rory Gibb&
#> Maxwell J Farrell. (2025). The Global Virome in One Network (VIRION): Data
#> Package [Dataset]. Zenodo. https://doi.org/10.5281/zenodo.15643004
```

``` r

# sometimes you want a bibtex entry item
export_deposit_bibtex("15643004")
#> @dataset{cole_brookson_2025_15643004,
#>   author       = {Cole Brookson and
#>                   Collin Schwantes and
#>                   Timothée Poisot and
#>                   Tad Dallas and
#>                   Greg Albery and
#>                   Colin J. Carlson and
#>                   Cecilia A. Sanchez and
#>                   Renata Muylaert and
#>                   Evan Eskew and
#>                   Rory Gibb and
#>                   Maxwell J Farrell},
#>   title        = {The Global Virome in One Network (VIRION): Data
#>                    Package
#>                   },
#>   month        = jun,
#>   year         = 2025,
#>   publisher    = {Zenodo},
#>   doi          = {10.5281/zenodo.15643004},
#>   url          = {https://doi.org/10.5281/zenodo.15643004},
#> }
```

### What about the data sources?

The data sources used to create Virion are referenced directly in the
deposit metadata. These can be accessed by retrieving the deposit
metadata.

``` r


metadata_json_text <- export_deposit_metadata(zenodo_id = "15692263",format = "json",verbose = FALSE)

metadata_list <- jsonlite::fromJSON(metadata_json_text,)

related_identifiers <- metadata_list$metadata$related_identifiers

required_items_filter <- related_identifiers$relation_type$id == "requires"

required_items <- related_identifiers[required_items_filter,]

required_items$resource_type <- required_items$resource_type$id

required_items$relation_type <- "requires"

required_items |>
  kableExtra::kable() |>
  kableExtra::kable_material()
```

|  | identifier | relation_type | resource_type | scheme |
|:---|:---|:---|:---|:---|
| 3 | <https://ftp.ncbi.nlm.nih.gov/genomes/Viruses/AllNuclMetadata/> accessed on 2025-06-09 | requires | dataset | url |
| 4 | 10.5281/zenodo.5167655 | requires | dataset | doi |
| 5 | <https://catalog.data.gov/dataset/predict-animals-sampled-c593d> | requires | dataset | url |
| 6 | <https://ictv.global/sites/default/files/MSL/ICTV_Master_Species_List_2024_MSL40.v1.xlsx> | requires | dataset | url |
| 7 | <https://ictv.global/sites/default/files/VMR/VMR_MSL40.v1.20250307.xlsx> | requires | dataset | url |

## What do the fields in VIRION mean?

Great question! The datapackage.json file contains field descriptions.
It is fairly human readable but we can take a closer look using some
built in functions.

For a deeper dive, check out the
[frictionless](https://docs.ropensci.org/frictionless/) package.

``` r


# this function wraps frictionless functions and extracts 
dict_list <- get_data_dictionary(datapackage_json = "outputs/15692263/datapackage.json")

dict_list$detection_csv
#> # A tibble: 5 × 3
#>   name              type    description                                         
#>   <chr>             <chr>   <chr>                                               
#> 1 AssocID           number  Row number from current VIRION version              
#> 2 DetectionMethod   string  Four harmonized categories in descending order of s…
#> 3 DetectionOriginal string  Method used for determing the presence of a virus a…
#> 4 HostFlagID        boolean Denotes the presence of possible uncertainty in hos…
#> 5 NCBIAccession     string  A unique identifier assigned to a record in sequenc…
```

Use [`purrr::map`](https://purrr.tidyverse.org/reference/map.html) to
make a full data dictionary for a given deposit.

``` r


html_tables <- purrr::map(dict_list, function(x){
  html_tables <- kableExtra::kable(x = x) |>
  kableExtra::kable_material()
  
})

list_items <- names(dict_list)

tables <- sprintf("<h3>%s</h3><br>%s",list_items,html_tables) |>
  paste(collapse = "<br>")

cat(tables)
```

### detection_csv

  

| name | type | description |
|:---|:---|:---|
| AssocID | number | Row number from current VIRION version |
| DetectionMethod | string | Four harmonized categories in descending order of strength of evidence: “Isolation/Observation,” “PCR/Sequencing,” “Antibodies,” and “Not specified”. In some cases where detection method is not available via metadata, source information is used as DetectionOriginal (e.g., “NCBI Nucleotide”). |
| DetectionOriginal | string | Method used for determing the presence of a virus as described in the original work |
| HostFlagID | boolean | Denotes the presence of possible uncertainty in host identification, which users may want to check before proceeding any further. |
| NCBIAccession | string | A unique identifier assigned to a record in sequence databases such as GenBank |

  

### edgelist

  

| name | type | description |
|:---|:---|:---|
| HostTaxID | number | Taxonomic identification number from NCBI for host taxa. |
| VirusTaxID | number | Taxonomic identification number from NCBI for virus taxa. |
| AssocID | string | Row number from current VIRION version |

  

### provenance_csv

  

| name | type | description |
|:---|:---|:---|
| AssocID | number | Row number from current VIRION version |
| HostOriginal | string | Host name from original dataset |
| VirusOriginal | string | Virus name from original dataset |
| Database | string | Source for the record. One of EID2, Shaw, HP3, GMPD2, PREDICT, OR GenBank |
| DatabaseVersion | string | For static data, a citation. For dynamic data (e.g. Genbank) the access URL and a time stamp |
| ReferenceText | string | A text description of literature sources |
| PMID | number | PubMed identifiers for literature sources |

  

### taxonomy_host

  

| name | type | description |
|:---|:---|:---|
| HostTaxID | number | Taxonomic identification number from NCBI for host taxa. |
| Host | string | Host species name |
| HostGenus | string | Host genus name |
| HostFamily | string | Host family name |
| HostOrder | string | Host order name |
| HostClass | string | Host class name |
| HostNCBIResolved | boolean | Indicates whether or not the host taxa could harmonized with the NCBI taxonomy. |

  

### taxonomy_virus

  

| name | type | description |
|:---|:---|:---|
| VirusTaxID | number | Taxonomic identification number from NCBI for virus taxa. |
| Virus | string | Virus species name |
| VirusGenus | string | Virus genus name |
| VirusFamily | string | Virus family name |
| VirusOrder | string | Virus order name |
| VirusClass | string | Virus class name |
| VirusNCBIResolved | boolean | Indicates whether or not the virus taxa could harmonized with the NCBI taxonomy. |
| ICTVRatified | boolean | Indicates whether or not the virus taxa is ratified by the nternational Committee on Taxonomy of Viruses (ICTV). |
| Database | string | Source for the record. One of EID2, Shaw, HP3, GMPD2, PREDICT, OR GenBank |

  

### temporal_csv

  

| name | type | description |
|:---|:---|:---|
| AssocID | number | Row number from current VIRION version |
| PublicationYear | number | For literature derived records. PublicationYear provides the year the literature source was published, accessed either from the original database’s reference description or from scraping the PubMed database. |
| ReleaseYear | number | The year a given association was “released” in public information (EID2 and PREDICT) or a publicly deposited sample on GenBank. For PREDICT, all values are given as 2021, given the release of a static file at that time even though some findings may have been published or deposited in GenBank earlier. (This redundancy should be captured in overlap with GenBank and EID2.) |
| ReleaseMonth | number | The month a given association was “released” to the public |
| ReleaseDay | number | The day a given association was “released” to the public |
| CollectionYear | number | Reports the year of actual sample collection (GenBank and Predict) |
| CollectionMonth | number | Reports the month of actual sample collection (GenBank and Predict) |
| CollectionDay | number | Reports the day of actual sample collection (GenBank and Predict) |

  

### virion_csv

  

| name | type | description |
|:---|:---|:---|
| Host | string | Host species name |
| Virus | string | Virus species name |
| HostTaxID | number | Taxonomic identification number from NCBI for host taxa. |
| VirusTaxID | number | Taxonomic identification number from NCBI for virus taxa. |
| HostNCBIResolved | boolean | Indicates whether or not the host taxa could harmonized with the NCBI taxonomy. |
| VirusNCBIResolved | boolean | Indicates whether or not the virus taxa could harmonized with the NCBI taxonomy. |
| ICTVRatified | boolean | Indicates whether or not the virus taxa is ratified by the nternational Committee on Taxonomy of Viruses (ICTV). |
| HostGenus | string | Host genus name |
| HostFamily | string | Host family name |
| HostOrder | string | Host order name |
| HostClass | string | Host class name |
| HostOriginal | string | Host name from original dataset |
| VirusGenus | string | Virus genus name |
| VirusFamily | string | Virus family name |
| VirusOrder | string | Virus order name |
| VirusClass | string | Virus class name |
| VirusOriginal | string | Virus name from original dataset |
| HostFlagID | boolean | Denotes the presence of possible uncertainty in host identification, which users may want to check before proceeding any further. |
| DetectionMethod | string | Four harmonized categories in descending order of strength of evidence: “Isolation/Observation,” “PCR/Sequencing,” “Antibodies,” and “Not specified”. In some cases where detection method is not available via metadata, source information is used as DetectionOriginal (e.g., “NCBI Nucleotide”). |
| DetectionOriginal | string | Method used for determing the presence of a virus as described in the original work |
| Database | string | Source for the record. One of EID2, Shaw, HP3, GMPD2, PREDICT, OR GenBank |
| DatabaseVersion | string | For static data, a citation. For dynamic data (e.g. Genbank) the access URL and a time stamp |
| PublicationYear | number | For literature derived records. PublicationYear provides the year the literature source was published, accessed either from the original database’s reference description or from scraping the PubMed database. |
| ReferenceText | string | A text description of literature sources |
| PMID | number | PubMed identifiers for literature sources |
| ReleaseYear | number | The year a given association was “released” in public information (EID2 and PREDICT) or a publicly deposited sample on GenBank. For PREDICT, all values are given as 2021, given the release of a static file at that time even though some findings may have been published or deposited in GenBank earlier. (This redundancy should be captured in overlap with GenBank and EID2.) |
| ReleaseMonth | number | The month a given association was “released” to the public |
| ReleaseDay | number | The day a given association was “released” to the public |
| CollectionYear | number | Reports the year of actual sample collection (GenBank and Predict) |
| CollectionMonth | number | Reports the month of actual sample collection (GenBank and Predict) |
| CollectionDay | number | Reports the day of actual sample collection (GenBank and Predict) |
| AssocID | number | Row number. Used as an id. Will be specific to a given version of the data |
| DatabaseDOI | string | Persistent digital identifer for the database |
| Release_Date | date | Date data were released |
| Collection_Date | string | Date of actual sample collection |
| NCBIAccession | string | A unique identifier assigned to a record in sequence databases such as GenBank |
