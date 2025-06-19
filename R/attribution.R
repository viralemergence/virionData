#' Get bibtex for citaiton
#'
#' @param zenodo_id Character
#' @param verbose Logical
#'
#' @returns Character. bibtex formatted entry
#' @export
#'
#' @examples
#' \dontrun{
#' get_version_bibtex(zenodo_id ) |>
#'   writeLines(con = "outputs/citation.bib")
#' }
get_version_bibtex <- function(zenodo_id = the$working_version,verbose = TRUE){

  zenodo_id <- sanitize_version(zenodo_id)

  bibtex_url <- sprintf("https://zenodo.org/records/%s/export/bibtex",zenodo_id)

  content <- httr::GET(bibtex_url)
  bibtex_text <- httr::content(content,as = "text")

  if(verbose){
    cat(bibtex_text)
  }

  invisible(bibtex_text)
}


#' Get Deposit Citation
#'
#' @param zenodo_id Character. Version or Deposit ID from Zenodo
#' @param style Character. One of "havard-cite-them-right",
#' "apa",
#' "modern-language-association",
#' "vancouver",
#' "chicago-fullnote-bibliography", or
# '"ieee"
#' @param verbose Logical. Print the citation?
#'
#' @returns Character. Text for a citation
#' @export
#'
#' @examples
#'
#' \dontrun{
#' get_version_citation(zenodo_id = "15692263",
#'                     style = "apa")
#' }
#'
get_version_citation <- function(zenodo_id = the$working_version,
                                 style = c("havard-cite-them-right",
                                           "apa",
                                           "modern-language-association",
                                           "vancouver",
                                           "chicago-fullnote-bibliography",
                                           "ieee"),
                                 verbose = TRUE){

  zenodo_id <- sanitize_version(zenodo_id)

  citation_url <- sprintf("https://zenodo.org/api/records/%s?locale=en-US&style=%s",
                          zenodo_id,style)

  citation_content <- httr::GET(citation_url, httr::accept("text/x-bibliography"))

  citation_text <- httr::content(citation_content,
                                 type = "text/x-bibliography",
                                 encoding = "UTF-8")

  if(verbose){
    cat(citation_text)
  }

  invisible(citation_text)
}
