#' Export Deposit Metadata in various formats
#'
#'
#' @param zenodo_id String. ID for a Zenodo deposit. Should correspond to the version of a deposit.
#' @param format String. File format for export. One of "json", "json-ld","csl","datacite-json","datacite-xml",
#' "dublincore","marcxml","bibtex","geojson","dcat-ap","codemeta", or "cff"
#' @param verbose Logical. Print the formatted metadata?
#'
#' @returns Character. Returns text in selected format.
#' @export
#'
#' @examples
#'  \dontrun{
#' export_deposit_metadata(zenodo_id = "15692263","json-ld" ) |>
#'   writeLines(con = "outputs/citation.jsonld")
#' }
export_deposit_metadata <- function(zenodo_id = the$working_version, format, verbose = TRUE){

  formats <- c("json", "json-ld","csl","datacite-json","datacite-xml", "dublincore","marcxml","bibtex","geojson","dcat-ap","codemeta", "cff")

  assertthat::assert_that(length(format) ==1)
  assertthat::assert_that(format %in% formats)


  zenodo_id <- sanitize_version(zenodo_id)
  export_url <- sprintf("https://zenodo.org/records/%s/export/%s",zenodo_id,format)
  content <- httr::GET(export_url)
  content_text <- httr::content(content,as = "text",encoding = "UTF-8")

  if(verbose){
    cat(content_text)
  }

  invisible(content_text)
}

#' Get bibtex entry for a deposit.
#'
#' Bibtex specific wrapper for export_deposit_metadata
#'
#' @inheritParams export_deposit_metadata
#'
#' @returns Character. bibtex formatted entry
#' @export
#'
#' @examples
#' \dontrun{
#' export_deposit_bibtex(zenodo_id = "15692263" ) |>
#'   writeLines(con = "outputs/citation.bib")
#' }
export_deposit_bibtex <- function(zenodo_id = the$working_version,verbose = TRUE){

  out <- export_deposit_metadata(zenodo_id = zenodo_id,format = "bibtex",verbose = verbose)
  invisible(out)
}


#' Get Version Citation
#'
#' @param zenodo_id String. ID for a Zenodo deposit. Should correspond to the version of a deposit.
#' @param style Character. One of "havard-cite-them-right",
#' "apa",
#' "modern-language-association",
#' "vancouver",
#' "chicago-fullnote-bibliography", or
#' "ieee"
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


  styles <-  c("havard-cite-them-right",
               "apa",
               "modern-language-association",
               "vancouver",
               "chicago-fullnote-bibliography",
               "ieee")

  assertthat::assert_that(length(style) == 1)
  assertthat::assert_that(style %in% styles)




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
