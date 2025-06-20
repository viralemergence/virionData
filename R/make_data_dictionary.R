#' Get Data Dictionary for a data Package
#'
#' Creates a list of data frames that constitutes the data dictionary.
#'
#' @param datapackage_json Character. Path to datapackage.json file
#'
#' @returns List. Named list of data frames with the following fields:
#' * name = field name
#' * type = field type
#' * description = characterization of the field
#' @export
#'
#' @examples
#'
#' \dontrun{
#' get_data_dictionary(datapackage_json = "data/1235600/datapackage.json")
#' }
#'
get_data_dictionary <- function(datapackage_json){

  assertthat::assert_that(fs::file_exists(datapackage_json))

  pkg_json <- frictionless::read_package(file = datapackage_json)

  resources <- pkg_json$resources

  resource_names <- purrr::map_chr(resources, get_resource_name)
  resource_names_clean <- gsub("\\.",replacement = "_", resource_names)
  resources <- purrr::set_names(x = resources,resource_names_clean)

  out <- purrr::map(resources,get_individual_dictionary )

  return(out)

}

#' Get the dictionary for a single resource
#'
#' @param x List. Resource object from the data package.
#'
#' @returns dataframe with three fields.
#' @export
#'
get_individual_dictionary <- function(x){

  out <- x$schema$fields |>
    purrr::map_dfr(\(x){x})

  return(out)
}

#' Get the name of a resource
#'
#' @param x data frame with name field
#'
#' @returns character. Name of resources
get_resource_name <- function(x){
  x$name
}
