#' Create advanced dictionary
#'
#' This is an extended/advanced version of the dicionary created by `homodatum::create_dic`.
#' Further down the line, this will be moved to `homodatum`, potentially integrated in the
#' existing dictionary function by `homodatum::create_dic(..., extended = TRUE)`.
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
create_dic <- function(d, frtype = NULL, extended = FALSE){
  dic <- homodatum::create_dic(d = d, frtype = frtype)

  if(extended == TRUE){
    extended_info <- d %>%
      dplyr::mutate_if(is.factor, as.character) %>%
      purrr::map_df( ~ (data.frame(
        n_distinct = dplyr::n_distinct(.x),
        unique = ifelse(dplyr::n_distinct(.x) == length(.x), TRUE, FALSE),
        class = class(.x),
        mean = ifelse(is.numeric(.x), mean(.x, na.rm = TRUE), NA),
        median = ifelse(is.numeric(.x), median(.x, na.rm = TRUE), NA),
        min = ifelse(is.numeric(.x), min(.x, na.rm = TRUE), NA),
        max = ifelse(is.numeric(.x), max(.x, na.rm = TRUE), NA),
        missings = sum(is.na(.x)),
        stringsAsFactors = FALSE)),
        .id = "id")

    dic <- dic %>%
      dplyr::inner_join(extended_info)

  }

  dic
}


#' Validate dataframe for requirements
#'
#' Validates whether a given dataframe of list of dataframes meets the requirements
#' specified in `requirements`
#'
#' @param x Dataframe or list of dataframes to be validated
#' @param requirements List of requirements to be validated
#'
#' @return List object which returns for each requirement whether it x satisfies it,
#' using label param passed as part of requirements
#'
#' @export
#'
#' @examples
validate_requirements <- function(x, requirements){
  output <- list()
  output$table <- validate_table_meta(x, requirements)
  output$specs <- validate_table_specs(x, requirements)
  output$fields <- validate_fields(x, requirements)

  output
}


