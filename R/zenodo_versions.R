### list all versions of deposit on zenodo

# https://zenodo.org/api/records/15257971/versions



#' Get Versions of a Deposit on Zenodo
#'
#' This function gets the metadata for all the versions of a deposit associated
#' with a parent id. The parent id is used to identify a set of works that are
#' different versions of the same work. The parent id is provided by the
#' Zenodo API. To find the parent ID, download a JSON representation of the
#' deposit (export to json on the webpage or use `export_deposit_metadata`),
#' there will be an attribute called parent that looks like
#' "https://zenodo.org/api/records/15020049".
#' The 8 digit string at the end of the url is the parent id.
#'
#' This function also sets package variables
#'
#' @param parent_id String. Identifier for a Zenodo deposit with multiple versions.
#'
#' @returns Data frame. Invisible. The data frame contains the Zenodo id for each version of
#' the deposit, as well as the version name, and logical field called latest that
#' indicates if this is the latest version.
#'
#' @export
#'
list_deposit_versions <- function(parent_id = "15643003"){

  assertthat::assert_that(is.character(parent_id))

  parent_url <- sprintf("https://zenodo.org/api/records/%s",parent_id)

  parent_json <- rlang::try_fetch(jsonlite::fromJSON(txt = parent_url),
                                  error = function(cnd) rlang::abort("Failed.", parent = cnd),
                                  warning = function(cnd) rlang::abort("Failed", parent = cnd))


  latest_id <- as.character(parent_json$id)

  versions_json <- jsonlite::fromJSON(parent_json$links$versions)

  zenodo_id <- as.character(versions_json$hits$hits$id)
  versions_metadata <- versions_json$hits$hits

  versions_metadata$latest_version <- purrr::map_lgl(versions_metadata$metadata$relations$version, \(x){
    x$is_last
  })

  the$latest_version <- latest_id
  the$all_versions <- zenodo_id

  summary_df <- versions_metadata[c("id","latest_version","doi_url")]
  summary_df$publication_date <- versions_metadata$metadata$publication_date
  the$summary_df <- summary_df[c("id","latest_version","publication_date","doi_url")]

  invisible(versions_metadata)
}

#' Simple summary data frame for the deposit
#'
#' @returns data frame. Simplified deposit metadata with the following fields:
#' * id = zenodo id
#' * latest_version = True or False indicating if that record is the latest version of the dataset.
#' * publication_date = YMD the item was published to zenodo
#' * doi_url = digital object identifier for that version of the record.
#' @export
#'
#' @examples
#' deposit_summary()
#'
deposit_summary <- function(){
  the$summary_df
}

#' Download deposit version
#'
#' Downloads and extracts some version of the deposit.
#'
#' @param zenodo_id String. ID for a Zenodo deposit. Should correspond to the version of a deposit.
#' @param dir_path String. Path to directory where the files should be downloaded
#'  e.g. "inst/extdata/wdds_archive" note no trailing slash on the path.
#' @param deposit_versions data frame
#'
#' @returns String. Path to downloaded version.
#' @export
#'
download_deposit_version <- function(zenodo_id, deposit_versions = list_deposit_versions(), dir_path){

  ## clean up zenodo id
  zenodo_id <- sanitize_version(zenodo_id)

  ## create folder in archive for version
  version_dir <- fs::path(dir_path,zenodo_id)

  msg <- sprintf("deposit will download to %s",version_dir)

  message(msg)

  fs::dir_create(path = version_dir)

  ## use id to get the thing
  version_files <- deposit_versions[deposit_versions$id == zenodo_id,"files"][[1]]

  # api_url <- sprintf("https://zenodo.org/api/records/%s",zenodo_id)
  #
  # id_json <- jsonlite::fromJSON(api_url)

  deposit_file  <- fs::path_file(version_files$key)

  download_path <- sprintf("%s/%s",version_dir,deposit_file)

  ## add check for version in dir?
  for(i in 1:length(version_files$links$self)){
    utils::download.file(url = version_files$links$self[i], destfile = download_path[i])
  }


  return(version_dir)
}

#' Batch download deposit versions
#'
#' This is `download_deposit_version` wrapped in a `purr::map` call.
#'
#' @param dir_path Character. Path to folder where files should be downloaded.
#' @param zenodo_ids Character. Either a vector of zenodo ids or "all"
#'
#' @returns List of download locations.
#' @export
#'
#' @examples
#' \dontrun{
#' # get all deposit versions
#' batch_download_deposit_versions(dir_path = "outputs")
#'
#' # get select versions
#' batch_download_deposit_versions(zenodo_ids = c(15677137", "15643004"), dir_path = "outputs)
#' }
#'
batch_download_deposit_versions <- function(zenodo_ids = "all", dir_path){

  deposit_versions = list_deposit_versions()
  if(zenodo_ids == "all"){
    zenodo_ids <- the$all_versions
  }

  ## map over the version and add to archive
  out  <- purrr::map_chr(zenodo_ids,function(x){
    download_deposit_version(zenodo_id = x,
                             deposit_versions = deposit_versions,
                             dir_path = dir_path)})

  return(out)

}

#' Check version ids
#'
#' Checks that version ids are properly formatted. IDs should
#'  either be integers ("15643003") OR "latest"
#'
#' @param version Character. Version identifier.
#'
#' @returns Character. Version identifier from Zenodo.
#' @export
#'
#' @examples
#'
#' sanitize_version("latest")
#' sanitize_version(" 15643003")
#'
sanitize_version <- function(version){

  # auto converts to character
  version_nows <- trimws(version,which = "both")

  if(version_nows == "latest"){
    version_nows <- the$latest_version
    if(version_nows == ""){
      list_deposit_versions()
      version_nows <- the$latest_version
    }
  }

  verify_integer <- grepl(pattern = "^[0-9]+$",x = version_nows)

  if(verify_integer){
    return(version_nows)
  }

  msg <- sprintf("version (%s) is not an interger.",version_nows)
  rlang::abort(msg)
}



#' Get a specific version of the data
#'
#' @param version Character. identifier for a version e.g. "15643003" or "latest"
#' @param style  Charater.
#' @param dir_path Character.
#' @param refresh_deposits_versions Logical.
#' @param verbose Logical.
#'
#' @returns Character. path to versioned data.
#' @export
#'
get_versioned_data <- function(version = "latest", style = "apa",dir_path, refresh_deposits_versions = TRUE, verbose = TRUE){

  if(refresh_deposits_versions || the$all_versions == ""){
    list_deposit_versions()
  }

  # sanitize version
  version <- sanitize_version(version)

  # set env variables
  the$working_version <- version

  # citation
  get_version_citation(style = style,verbose = verbose)

  # download data
  out <- download_deposit_version(zenodo_id = version,dir_path = dir_path)

  return(out)

}


