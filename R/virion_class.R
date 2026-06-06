## virion class
## cleaner than setting package envs
## better alignment with py-virion-data


deposit <- R6::R6Class("deposit",
                       public = list(
                         parent_id = "",
                         parent_url = "",
                         parent_json = "",
                         latest_version = "",
                         versions_metadata = "",
                         working_version = "",
                         working_url = "",
                         working_json = "",
                         working_bibtex = "",
                         working_citation = "",
                         # working files consist of key (file name) and url (url to file on zenodo)
                         working_files = data.frame(),
                         all_versions = "",
                         summary_df = data.frame(),
                         ### init deposit ####
                         initialize = function(set_working_version_to_latest, parent_id = "15643003") {

                           assertthat::assert_that(is.character(parent_id))

                           self$parent_id <- parent_id
                           self$parent_url <- sprintf("https://zenodo.org/api/records/%s",parent_id)

                           # get the parent json
                           self$parent_json <- get_json(self$parent_url)

                           # get the latest id
                           latest_id <- as.character(self$parent_json$id)

                           versions_json <- get_json(self$parent_json$links$versions)

                           self$all_versions <- as.character(versions_json$hits$hits$id)
                           versions_metadata <- versions_json$hits$hits

                           versions_metadata$latest_version <- purrr::map_lgl(versions_metadata$metadata$relations$version, \(x){
                             x$is_last
                           })

                           self$latest_version <- latest_id

                           summary_df <- versions_metadata[c("id","latest_version","doi_url")]
                           summary_df$publication_date <- versions_metadata$metadata$publication_date
                           self$summary_df <- summary_df[c("id","latest_version","publication_date","doi_url")]
                           self$versions_metadata <- versions_metadata

                           invisible(self)
                         },
                         ### set working version ####
                         set_working_version = function(zenodo_id, style = "apa",
                                                        print_bibtex = TRUE,
                                                        print_citation = TRUE) {
                           zenodo_id <- self$check_zenodo_id(zenodo_id)
                           self$working_version <- zenodo_id
                           self$working_url <- make_url(id = zenodo_id)
                           self$working_json <- get_json(self$working_url)
                           # add files to a dataframe
                           dep_files = self$working_json$files
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
                         download_versioned_data = function(zenodo_id = "working",
                                                            dir = "outputs",
                                                            recreate = TRUE){
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
                         check_zenodo_id = function(zenodo_id){
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
                         export_metadata = function(zenodo_id,format,  verbose){
                           export_deposit_metadata(zenodo_id,format,verbose)
                           }

                       )
)
