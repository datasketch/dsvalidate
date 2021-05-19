#' Create advanced dictionary
#'
#' An extended/advanced version of the dicionary created by homodatum::create_dic
#' This could be part of homodatum, potentially integrated in the existing dictionary
#' function by homodatum::create_dic(..., extended = TRUE).
#'
#' Additionally to the existing id, label, and hdType this should have
#' - the base R class of the variable (numeric, character, ...)
#' - number of distinct categories (for Cat, NULL otherwise)
#' - min, max, mean, median values (for Num, NULL otherwise)
#' - number format, i.e. decimal separator (for Num, NULL otherwise)
#' - number of missing values
#' - anything else?
#'
#' @param df
#'
#' @return Dataframe with dictionary for df
#' @export
#'
#' @examples
create_dic_advanced <- function(df){

}


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
#' Example 1: Need 2+ columns in total AND need at least one categorical column.
#' The logic behind the requirements in the yaml file is:
#' - requirement 1: number of columns needs to be greater than 1
#' - requirement 2: number of columns where hdType is equal to Cat needs to be greater than 0
#' req1 <- yaml::read_yaml("inst/test_dsvalidate/ex01-two-columns-one-categorical/requirements.yaml")
#'
#' Example 2: Need 2+ columns in total with fewer than 20 categories each
#' The logic behind the requirements in the yaml file is:
#' - number of columns where hdType is equal to Cat and n_distinct is less than 20 needs to be greater than 1
#' req2 <- yaml::read_yaml("inst/test_dsvalidate/ex02-two-categorical-cols-max-distinct-cat/requirements.yaml")
requirements_load <- function(path = "dsvalidate"){

}


#' Validate dataframe for requirements
#'
#' Validates whether a given df has the necessary requirements.
#'
#' @param df Dataframe to be validated
#' @param requirements List of requirements to be validated
#'
#' @return List object which returns for each requirement whether it df satisfies it,
#' using label param passed as part of requirements
#'
#' @export
#'
#' @examples
validate_requirements <- function(df, requirements){

}


#' Recommends suitable visualization
#'
#' ADVANCED
#'
#' Suggest suitable visualization types from a loaded df.
#'
#' For this, we would need a list of all possible visualisation we offer
#' with DS apps and sets of minimum requirements for each of them.
#'
#' e.g. app-sankey-interactive: 2+ columns with categorical variables with max 20 categories (more?)
#'      app-simple-charts-orgs: 2+ columns in total AND need at least one categorical column (more?)
#'
#' @param df Loaded dataframe
#'
#' @return List of recommended visualizations
#' @export
#'
#' @examples
recommend_viz <- function(df){

}


#' Recommends suitable app
#'
#' ADVANCED
#'
#' Suggest available DS apps from a loaded df.
#'
#' This uses recommend_viz to suggest the DS apps that can be used
#' to create recommended visualisations from the loaded df.
#'
#' @param df Loaded dataframe
#'
#' @return List of recommended DS apps
#' @export
#'
#' @examples
recommend_app <- function(df){

}

