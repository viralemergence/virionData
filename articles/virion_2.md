# Working with Virion 2.0

``` r

library(virionData)
```

On 28 April 2026, Virion 2.0 was released with the goal of streamlining
the processing pipeline. Starting with deposit version `19837096`, the
data structure diverges significantly from the [original
manuscript](https://journals.asm.org/doi/10.1128/mbio.02985-21) and
previous versions of the data.

The major difference is that taxonomic data, database metadata, and NCBI
accession numbers are now stored in separate tables to avoid unnecessary
operations on those data and to avoid repeating information in the
virion table (data normalization).

``` r


# create a new virion deposit object
virion_deposit <- deposit$new()

# set the working version to latest version of virion.
# it is always a better idea to set the version explicitly.
virion_deposit$set_working_version(print_citation = FALSE,print_bibtex = FALSE)

# get the data
## check the file names
# virion_deposit$working_files$file_key
# 

tax_table <- virion_deposit$load_remote_csv_file(file_key = "tax_table.csv.gz",col_types = "c", refresh = FALSE)
#> downloading file

db_table <- virion_deposit$load_remote_csv_file(file_key = "db_table.csv",col_types = "c")
#> downloading file


virion_data <- virion_deposit$load_remote_csv_file(file_key = "virion.csv.gz",col_types = "c")
#> downloading file

ncbi_accession <- virion_deposit$load_remote_csv_file(file_key = "ncbi_accession.csv.gz",col_types = "c")
#> downloading file

# join host tax data
 virion_data_host <- dplyr::left_join(x = virion_data,
                   y = tax_table |>
                      dplyr::rename_with(~ paste0("Host",
                                                  .x,
                                                  recycle0 = TRUE)
                                         ),
                   by = c("HostTaxHashID"))
 
 # join virus tax data
 virion_data_tax <- dplyr::left_join(x = virion_data_host,
                   y = tax_table |>
                      dplyr::rename_with(~ paste0("Virus",
                                                  .x,
                                                  recycle0 = TRUE)
                                         ),
                   by = c("VirusTaxHashID")) |>
   dplyr::mutate(AssocID = as.character(AssocID))

# join NCBI accession numbers
 
virion_data_ncbi <- dplyr::left_join(
  x = virion_data_tax,
  y = ncbi_accession,
  by = "AssocID"
)

# join database data

virion_data_full <- dplyr::left_join(x = virion_data_ncbi,
                                   y = db_table,
                                   by = "DatabaseVersion" )

# if you need the exact same number of columns you could drop
# host and virus tax hash id columns.
```
