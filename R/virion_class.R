## virion class
## cleaner than setting package envs
## better alignment with py-virion-data


#' @title deposit
#'
#' @description An R6 client for getting virion data and metadata.
#'
#'
#' @return A `deposit` class (R6 class)
#' @examples
#' \dontrun{
#' # make a client
#' virion_deposit <- deposit$new()
#'
#' # methods
#' virion_deposit$set_working_version("latest")
#' virion_deposit$working_files
#'
#' # load remote data directly into your session
#' virion_data  <- virion_deposit$load_remote_csv_file(file_key = "virion.csv.gz")
#'
#' # download remote data to disk - defaults to working version
#' virion_deposit$download_versioned_data()
#'
#' }
#' @export
#' @import R6
#' @import readr
#'



deposit <- R6::R6Class("deposit",
                       public = list(
                         #' @field parent_id String. Identifier for a Zenodo deposit with multiple versions.
                         parent_id = "",
                         #' @field all_versions Character. All zenodo ids for versions of the deposit.
                         all_versions = "",
                         #' @field summary_df Data frame. A table of all zenodo ids for versions of the deposit.
                         summary_df = data.frame(),
                         #' @field latest_version String. Identifier for the latest version of the zenodo deposit.
                         latest_version = "",
                         #' @field working_version String. Identifier for the version of the zenodo deposit you are working with.
                         working_version = "",
                         #' @field working_bibtex String. Bibtex for the version of the zenodo deposit you are working with.
                         working_bibtex = "",
                         #' @field working_citation String. Citation for the version of the zenodo deposit you are working with.
                         working_citation = "",
                         #' @field working_files Data frame. Working files consist of key (file name) and url (url to file on zenodo)
                         working_files = data.frame(),
                         #' @field working_json list. Metadata for the working version.
                         working_json = list(),

                         #' @description Create a new `deposit` object, as an [R6][R6::R6-package]
                         #' client. This object is connected to the zenodo deposit for virion
                         #' and is the gateway for accessing different versions of the data.
                         #'
                         #' @param parent_id String. Identifier for a Zenodo deposit with multiple versions.
                         #'
                         #' @return A new `deposit` object
                         #' @examples
                         #' \dontrun{
                         #' virion_deposit <- deposit$new()
                         #' }

                         initialize = function( parent_id = "15643003") {

                           assertthat::assert_that(is.character(parent_id))

                           self$parent_id <- parent_id
                           parent_url <- sprintf("https://zenodo.org/api/records/%s",parent_id)

                           # get the parent json
                           parent_json <- get_json(parent_url)

                           # get the latest id
                           latest_id <- as.character(parent_json$id)

                           versions_json <- get_json(parent_json$links$versions)

                           ## multiple pages of versions are returned so must
                           # iterate over them

                           versions_metadata <- purrr::map_df( versions_json$links,function(x){
                             json_x <- get_json(x)

                             out <- (json_x$hits$hits)
                             return(out)
                           }
                           )


                           self$all_versions <- versions_metadata |>
                             dplyr::pull(id) |>
                             as.character()

                           # latest version is always on first page so does not need to change
                           versions_metadata$latest_version <- purrr::map_lgl(versions_metadata$metadata$relations$version, \(x){
                             x$is_last
                           })

                           self$latest_version <- latest_id

                           summary_df <- versions_metadata[c("id","latest_version","doi_url")]
                           summary_df$publication_date <- versions_metadata$metadata$publication_date
                           self$summary_df <- summary_df[c("id","latest_version","publication_date","doi_url")]

                           invisible(self)
                         },

                         #' @description Set the version of the data you would
                         #' like to work with. Note that the structure of Virion
                         #'  data may change through time.
                         #'
                         #' @param zenodo_id String. A zenodo id or latest.
                         #' @param style String. The citation style to be used.
                         #' @param print_bibtex Logical. Should the bibtex for the working version of the deposit be printed when setting the working version?
                         #' @param print_citation Logical. Should the citation for the working version of the deposit be printed?
                         #'
                         #' @returns invisibly returns self
                         #' @export
                         #'
                         #' @examples
                         #'
                         #' \dontrun{
                         #' # set the working version to the latest version of the deposit
                         #' virion_deposit$set_working_version("latest")
                         #'
                         #' # set the working versino to an arbitrary deposit
                         #' virion_deposit$all_versions
                         #' virion_deposit$set_working_version("19502921")
                         #' }
                         #'
                         set_working_version = function(zenodo_id = "latest",
                                                        style = "apa",
                                                        print_bibtex = TRUE,
                                                        print_citation = TRUE) {
                           zenodo_id <- self$check_zenodo_id(zenodo_id)
                           self$working_version <- zenodo_id
                           working_url <- make_url(id = zenodo_id)
                           working_json <- get_json(working_url)
                           # add files to a dataframe
                           dep_files = working_json$files
                           file_df <- data.frame(file_key = "", file_url = "")

                           for(i in 1:nrow(dep_files)){
                             item <- dep_files[i,]

                             file_key <- item$key
                             file_url <- item$links$self
                             file_df[i,] <- c(file_key,file_url)
                           }
                           self$working_files <- file_df
                           self$working_bibtex <- export_deposit_bibtex(zenodo_id = self$working_version,
                                                                        verbose = print_bibtex)
                           self$working_citation <- get_version_citation(zenodo_id = self$working_version,
                                                                         style = style,
                                                                         verbose = print_citation)
                           self$working_json <- working_json

                           invisible(self)
                         },
                         ## stream files -- makes and deletes a temp file
                         #' @description Load a remote csv listed in the working files field
                         #' directly into the environment.
                         #'
                         #' Can handle either csv or csv.gz files. This function
                         #' downloads a temporary file to your machine, reads it,
                         #' then returns a dataframe.
                         #'
                         #' The local file cache is removed when your R session
                         #' restarts.
                         #'
                         #' Consider using the `download_versioned_data` to
                         #' save a persistent version of the virion dataset on
                         #' your machine.
                         #'
                         #' @param file_key String. A file key as seen in the `working_files` field
                         #' @param ... Additional arguments to pass to [readr::read_csv()]
                         #' @param refresh Logical. Should files be re-downloaded.
                         #'
                         #' @returns dataframe of remote csv file.
                         #' @export
                         #'
                         #' @examples
                         #' \dontrun{
                         #' ## see the working files
                         #' virion_deposit$working_files
                         #' virion_data <- virion_deposit$load_remote_csv_file("virion.csv.gz")
                         #'
                         #' }
                         load_remote_csv_file = function(file_key,
                                                         refresh = FALSE,
                                                         ... # additional arguments for read_csv
                         ){
                           # check for proper file extension
                           match.arg(arg = fs::path_ext(file_key),choices = c("csv","gz"))
                           # check the file is in the working files
                           match.arg(file_key,choices = self$working_files$file_key)

                           local_path <- private$load_remote_file(file_key = file_key,
                                                                  dir = tempdir(),
                                                                  refresh = refresh)

                           out <- readr::read_csv(file = local_path,...)

                           return(out)

                         },
                         ## download files
                         #' @description Download versioned data
                         #'
                         #' Downloads all the files for a particular version of
                         #' Virion to `dir/zenodo_id`.
                         #'
                         #' This function does not change the working version!
                         #'
                         #' @param zenodo_id String. A zenodo id, working or latest
                         #' @param dir String. Storage location for files
                         #' @param refresh Logical. Should files be re-downloaded.
                         #'
                         #' @returns String. Path to download location.
                         #' @export
                         #'
                         #' @examples
                         #' \dontrun{
                         #' # download files for the working version
                         #' virion_deposit$download_versioned_data()
                         #'
                         #' # download files for the latest version
                         #' virion_deposit$download_versioned_data("latest")
                         #'
                         #' # download files for some zenodo id
                         #' virion_deposit$download_versioned_data("19502921")
                         #' }
                         #'
                         download_versioned_data = function(zenodo_id = "working",
                                                            dir = "outputs",
                                                            refresh = FALSE){
                           # preserve the working version of the object
                           working_version <- self$working_version

                           if(zenodo_id!="working"){

                             # set the working id to zenodo_id
                             self$set_working_version(zenodo_id = zenodo_id,
                                                      print_bibtex = FALSE,
                                                      print_citation = FALSE)
                           }
                           # make the directory if it doesnt exist
                           version_dir <- fs::path(dir, self$working_version)
                           fs::dir_create(path = version_dir)
                           self$working_files$local_path <- ""

                           for(i in 1:nrow(self$working_files)){
                             item <- self$working_files[i,]

                             local <- private$load_remote_file(file_key =item$file_key,
                                                               dir = version_dir,
                                                               refresh = refresh )

                             # append local files to working files
                             self$working_files[i,"local_path"] <- local
                           }

                           #keep the working version the same
                           if(working_version != self$working_version){
                             self$set_working_version(zenodo_id = working_version,
                                                      print_bibtex = FALSE,
                                                      print_citation = FALSE)
                           }

                           return(version_dir)

                         },
                         ## load a persistent version of the file
                         ## assume working version is the version id
                         ## inform?
                         #' @description
                         #' Load a local version of the csv.
                         #'
                         #' Assumes you would like to load the file from
                         #' the current working version.
                         #'
                         #' If the csv file is not present, a persistent version
                         #' of the file will be downloaded to your machine.
                         #'
                         #' @param file_key string. Name of file from `working_files`
                         #' @param refresh logical. Should the file be re-downloaded?
                         #' @param ... Additional parameters to pass to [readr::read_csv()]
                         #'
                         #' @returns data.frame from [readr::read_csv()]
                         #' @export
                         #'
                         #' @examples
                         #' \dontrun{
                         #'
                         #' virion <- deposit$new()
                         #' virion$set_working_version()
                         #' virion$working_files$file_key
                         #' virion$load_local_csv_file("virion.csv.gz")
                         #' }
                         #'
                         load_local_csv_file = function(file_key,
                                                        refresh,
                                                        ...){

                           # should be one of csv or csv.gz
                           match.arg(fs::path_ext(file_key),choices = c("csv", "gz"))
                           # check the file is in the working files
                           match.arg(file_key,choices = self$working_files$file_key)

                           ## loads from the working version
                           if(self$working_version == ""){
                             rlang::abort("Set the working version")
                           }
                           ## check that there are local files in the working files
                           if(any(!"local_path" %in% names(self$working_files), refresh)){
                             self$download_versioned_data(refresh = refresh)
                           }
                           ## match to key
                           local_path <- self$working_files[self$working_files$file_key == file_key, "local_path"]

                           out <- readr::read_csv(local_path,...)

                           return(out)

                         },
                         ## check id
                         #' @description Check the zenodo ID
                         #'
                         #' Look for key words like working or latest and convert
                         #' them to integer ids OR pass the zenodo id directly
                         #' to `sanitize_id()`
                         #'
                         #' @param zenodo_id String. One of working, latest, or a zenodo id
                         #'
                         #' @returns String. cleaned zenodo id
                         #' @export
                         #'
                         #' @examples
                         #'
                         #' \dontrun{
                         #' virion_deposit$check_zenodo_id("latest")
                         #' }
                         check_zenodo_id = function(zenodo_id){

                           zenodo_id  <- trimws(zenodo_id)

                           if(zenodo_id == "working"){
                             zenodo_id = self$working_version
                           }
                           if( zenodo_id == "latest"){
                             zenodo_id = self$latest_version
                           }
                           # this forces all IDs to sanitized
                           if(!zenodo_id %in% c("latest","working")){
                             zenodo_id = sanitize_id(zenodo_id)
                           }

                           return(zenodo_id)
                         },
                         # export metadata
                         #' @description Export metadata for a version of the deposit.
                         #'
                         #' @param zenodo_id String. One of "latest","working", or a zenodo id
                         #' @param format String. One of "json", "json-ld","csl","datacite-json","datacite-xml", "dublincore","marcxml","bibtex","geojson","dcat-ap","codemeta", "cff"
                         #' @param verbose Logical. Print the metadata
                         #'
                         #' @returns metadata in the specificed format (invisible)
                         #' @export
                         #'
                         #' @examples
                         #'
                         #' \dontrun{
                         #'  virion_deposit$export_metadata("working")
                         #' }
                         #'
                         export_metadata = function(zenodo_id,format,  verbose){
                           formats <- c("json", "json-ld","csl","datacite-json","datacite-xml", "dublincore","marcxml","bibtex","geojson","dcat-ap","codemeta", "cff")

                           assertthat::assert_that(length(format) ==1)
                           assertthat::assert_that(format %in% formats)


                           zenodo_id <- self$check_zenodo_id(zenodo_id)
                           export_url <- sprintf("https://zenodo.org/records/%s/export/%s",zenodo_id,format)
                           content <- httr::GET(export_url)
                           content_text <- httr::content(content,as = "text",encoding = "UTF-8")

                           if(verbose){
                             rlang::inform(content_text)
                           }

                           invisible(content_text)
                         },
                         #' @description Create a list of data dictionaries.
                         #'
                         #' @param file_key string. Name of file from `working_files`
                         #' @param dir string. Name of directory
                         #' @param refresh refresh Logical. Should files be re-downloaded.
                         #'
                         #' @returns  List. Named list of data frames with the following fields:
                         #' * name = field name
                         #' * type = field type
                         #' * description = characterization of the field
                         #' @export
                         #'
                         #' @examples
                         #' \dontrun{
                         #' virion <- deposit$new()
                         #' virion$set_working_version()
                         #' virion$get_data_dictionary()
                         #'
                         #' }
                         #'
                         get_data_dictionary = function(file_key = "datapackage.json", dir = tempdir(), refresh){
                           ## loads from the working version
                           if(self$working_version == ""){
                             rlang::abort("Set the working version")
                           }

                           if(any(refresh,!"local_path" %in% names(self$working_files))){
                             # always load the file from remote if refresh is true or no local paths
                             local_path <- private$load_remote_file(file_key = file_key,
                                                                    dir = dir,
                                                                    refresh = refresh)
                           } else if("local_path" %in% names(self$working_files)){
                             # else grab the file from self if a local path is recorded
                             local_path <- self$working_files[self$working_files$file_key == file_key, "local_path"]
                           }

                           get_data_dictionary(datapackage_json = local_path)
                         }

                       ),
                       private = list(
                         # @description Downloads remote files to a specific location.
                         #
                         # @param file_key string. Name of file from [deposit$working_files]
                         # @param dir string. Name of directory
                         # @param refresh Logical. Should files be re-downloaded.
                         #
                         # @returns String. Path to local file
                         # @noRd
                         load_remote_file = function(file_key, dir, refresh ){

                           if(self$working_version == ""){
                             rlang::abort("set working version")
                           }

                           ## make sure that types are correct
                           assertthat::assert_that(
                             assertthat::is.string(file_key),msg = "file_key must be a string."
                           )
                           assertthat::assert_that(
                             assertthat::is.string(dir),msg = "dir must be a string."
                           )

                           assertthat::assert_that(
                             assertthat::is.flag(refresh),msg = "refresh must be TRUE or FALSE."
                           )

                           match.arg(file_key,choices = self$working_files$file_key)

                           file_url <- self$working_files[self$working_files$file_key == file_key, "file_url"]

                           # make a directory in dir for the current working version
                           local_dir <- fs::path(dir,self$working_version)
                           fs::dir_create(local_dir)
                           # make a file path in that directory
                           local_path <- fs::path(local_dir, file_key)

                           out  <- download_refresh(file_url,local_path,refresh)
                           return(out)
                         }
                       )


)


#' Dummy function to pass RMD check
#'
#' Apparently a known issue in R6 as the class is not created until install.
#'
#' @returns Character string explaining why this function exists.
#'
dummy_imports <- function() {
  R6::R6Class
  readr::read_csv
  return("this function is used to have CMD checks import R6 and readr packages")
}
