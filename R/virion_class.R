## virion class
## cleaner than setting package envs
## better alignment with py-virion-data


#' @title deposit
#'
#' @description An R6 client for getting virion data and metadata.
#'
#' @return A `deposit` class (R6 class)
#' @examples
#' \dontrun{
#' # make a client
#' virion_deposit <- deposit$new("")
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
                         ### init deposit ####
                         #' @description Create a new `deposit` object, as an \pkg{R6}
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

                           self$all_versions <- as.character(versions_json$hits$hits$id)
                           versions_metadata <- versions_json$hits$hits

                           versions_metadata$latest_version <- purrr::map_lgl(versions_metadata$metadata$relations$version, \(x){
                             x$is_last
                           })

                           self$latest_version <- latest_id

                           summary_df <- versions_metadata[c("id","latest_version","doi_url")]
                           summary_df$publication_date <- versions_metadata$metadata$publication_date
                           self$summary_df <- summary_df[c("id","latest_version","publication_date","doi_url")]

                           invisible(self)
                         },
                         ### set working version ####
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
                         set_working_version = function(zenodo_id,
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

                           invisible(self)
                         },
                         ## stream files -- makes and deletes a temp file
                         #' @description Load a remote csv directly into the environment
                         #'
                         #' Can handle either csv or csv.gz files
                         #'
                         #' @param file_key String. A file key as seen in the working files field
                         #' @param ... Additional arguments to pass to \link{readr::read_csv}
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
                                                         ... # additional arguments for read_csv
                         ){

                           url_file <- self$working_files[self$working_files$file_key == file_key, "file_url"]

                           print(url_file)

                           extension <- fs::path_ext(file_key)
                           local <- tempfile(fileext = extension)
                           curl::curl_download(url =  url_file,
                                               destfile = local)


                           out <- readr::read_csv(file = local,...)
                           unlink(local)

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
                         #'
                         #' @returns String. Path to downlod location.
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
                                                            dir = "outputs"){
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

                           for(i in 1:nrow(self$working_files)){
                             item <- self$working_files[i,]
                             local  <- fs::path(version_dir,item$file_key)

                             curl::curl_download(url =  item$file_url,
                                                 destfile = local)
                           }

                           #keep the working version the same
                           if(working_version != self$working_version){
                             self$set_working_version(zenodo_id = working_version,
                                                      print_bibtex = FALSE,
                                                      print_citation = FALSE)
                           }

                           return(version_dir)

                         },
                         ## check id
                         #' @description Check the zenodo ID
                         #'
                         #' Look for key words like working or latest and convert
                         #' them to interger ids OR pass the zenodo id directly
                         #' to \link{sanitize_id}
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
                           if(!zenodo_id %in% c("latest","working")){
                             zenodo_id = self$sanitize_id(zenodo_id)
                           }

                           return(zenodo_id)
                         },
                         ## sanitize id -- consider making this private as it should only be used as part of check_id
                         #' @description Sanitize a zenodo id
                         #'
                         #' Remove any white space and check that it conforms to the zenodo id pattern.
                         #'
                         #' @param zenodo_id String. A zenodo id.
                         #'
                         #' @returns String. A cleaned zenodo id.
                         #' @export
                         #'
                         #' @examples
                         #'\dontrun{
                         #' virion_deposit$sanitize_id(" 2948598")
                         #'}
                         sanitize_id = function(zenodo_id){

                           zenodo_id_chr  <- as.character(zenodo_id)
                           zenodo_id_tws <- trimws(zenodo_id_chr)

                           verify_integer <- grepl(pattern = "^[0-9]+$",x = zenodo_id_tws)

                           if(verify_integer){
                             assertthat::assert_that(is.character(zenodo_id_tws))
                             return(zenodo_id_tws)
                           }
                           msg <- sprintf("version (%s) is not an interger.",zenodo_id_tws)
                           rlang::abort(msg)
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
                         }

                       )
)
