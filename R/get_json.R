## get json in a way that fail elegantly

get_json <- function(url){

  json <- rlang::try_fetch(jsonlite::fromJSON(txt = url),
                           error = function(cnd) rlang::abort("Failed.", parent = cnd),
                           warning = function(cnd) rlang::abort("Failed", parent = cnd))
  return(json)
}

# make urls
make_url <- function(base_url = "https://zenodo.org/api/records/%s", id){
 out <- sprintf("https://zenodo.org/api/records/%s", id)
 return(out)
}


