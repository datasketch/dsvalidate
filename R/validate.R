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
