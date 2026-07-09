## get json in a way that fails elegantly

#' Get json
#'
#' [jsonlite::fromJSON()] wrapped in [rlang::try_fetch()] to make calls
#' smoother.
#'
#' @param url String. A url fed to [jsonlite::fromJSON()]
#'
#' @returns List. JSON as a list
#' @export
#'
#' @examples
#' \dontrun{
#' get_json("https://www.example.com/example.json")
#' }
#'
get_json <- function(url){
  assertthat::assert_that(assertthat::is.string(url),msg = "URL must be a string")
  json <- rlang::try_fetch(jsonlite::fromJSON(txt = url),
                           error = function(cnd) rlang::abort("Failed.", parent = cnd),
                           warning = function(cnd) rlang::abort("Failed", parent = cnd))
  return(json)
}

# make urls
#' Make Zenodo URLs
#'
#' Appends a zenodo id to the zenodo api url.
#'
#' @param base_url string. url for zenodo api
#' @param id string. zenodo id.
#'
#' @returns String. URL for zenodo api.
#' @export
#'
#' @examples
#' make_url(id = "1235600")
#'
make_url <- function(base_url = "https://zenodo.org/api/records/%s", id){

  assertthat::assert_that(assertthat::is.string(base_url),msg = "base_url must be a string")
  id_clean <- sanitize_id(id)

 out <- sprintf("https://zenodo.org/api/records/%s", id_clean)
 return(out)
}

#' Download or redownload data
#'
#' Downloads a file or re-downloads a file to refresh the cached files.
#'
#' @param file_url String. URL for file you'd like to download
#' @param local String. A character string with the name where the downloaded file is saved. Tilde-expansion is performed.
#' @param refresh Logical. Should saved files be refreshed.
#'
#' @returns String. Local File path
#' @export
#'
download_refresh <- function(file_url, local, refresh){


  assertthat::assert_that(
    assertthat::is.flag(refresh),msg = "refresh should be true or false"
  )


  if(refresh){
    rlang::inform("refreshing file cache")
    curl::curl_download(url =  file_url,
                        destfile = local)
  }else if(!fs::file_exists(local)){
    rlang::inform("downloading file")
    curl::curl_download(url =  file_url,
                        destfile = local)
  } else {
    rlang::inform("using cached file")
  }
  return(local)
}


