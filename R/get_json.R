## get json in a way that fail elegantly

#' Get json
#'
#' @param url String. A url fed to \link{jsonlite::fromJSON}
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
#' @param base_url
#' @param id
#'
#' @returns String. URL for zenodo api.
#' @export
#'
#' @examples
#' make_url(id = "some_id")
#'
make_url <- function(base_url = "https://zenodo.org/api/records/%s", id){
 out <- sprintf("https://zenodo.org/api/records/%s", id)
 return(out)
}


