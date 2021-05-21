#' Load requirements for validation
#'
#' Loads and checks the requirements.yaml file, raises necessary
#' errors and warnings if necessary, and prepares it for use by
#' validate_requirements.
#'
#' The list obtained from the requirements.yaml file is a list with a sublist
#' for each requirement. Sublists (requirements are connected with logical AND).
#'
#' TODO: Would be good to enable AND and OR logic for requirements
#'
#' @param path Path for the requirements.yaml file; default is 'dsvalidate'
#'
#' @return List of requirements
#' @export
#'
#' @examples
#' # Example 1: Need 2+ columns in total AND need at least one categorical column.
#' # The logic behind the requirements in the yaml file is:
#' # - requirement 1: number of columns needs to be greater than 1
#' # - requirement 2: number of columns where hdType is equal to Cat needs to be greater than 0
#' file_path <- system.file("test_dsvalidate", "ex01-two-columns-one-categorical","dsvalidate", package = "dsvalidate")
#' req1 <- yaml::read_yaml(file.path(file_path, "requirements.yaml"))
#' req1
#'
#' # Example 2: Need 2+ columns in total with fewer than 20 categories each
#' # The logic behind the requirements in the yaml file is:
#' # - number of columns where hdType is equal to Cat and n_distinct is less than 20 needs to be greater than 1
#' file_path <- system.file("test_dsvalidate", "ex02-two-categorical-cols-max-distinct-cat","dsvalidate", package = "dsvalidate")
#' req2 <- yaml::read_yaml(file.path(file_path, "requirements.yaml"))
#' req2
requirements_load <- function(path = "dsvalidate"){

}
