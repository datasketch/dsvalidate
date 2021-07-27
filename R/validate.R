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
  if(is.null(frtype)){
    if(is_hdtibble(d)){
      frtype <- hdtibble_frType(d)
    }else{
      frtype <- guess_frType(d)
    }
  }
  if(!is_frType(frtype))
    frtype <- frType(frtype)
  ids <- col_ids_from_name(names(d))
  dic <-tibble::tibble(id = ids,
                       label = names(d),
                       hdType = frType_hdTypes(frtype))

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


}


validate_table_meta <- function(x, requirements){

  output <- list(data_type = NULL,
                 n_tables = NULL,
                 table_ids = NULL)

  table_ids_expected <- names(requirements$table)
  n_tables_expected <- length(table_ids_expected)

  output$data_type <- validate_data_type(x = x,
                                         n_tables_expected = n_tables_expected)

  output$n_tables <- validate_n_tables(x = x,
                                       n_tables_expected = n_tables_expected)

  output$table_ids <- validate_table_ids(x = x,
                                         n_tables_expected = n_tables_expected,
                                         table_ids_expected = table_ids_expected,
                                         data_type_met = output$data_type$met)

  output
}


validate_data_type <- function(x, n_tables_expected){
  if(n_tables_expected > 1){
    if(is.data.frame(x)){
      data_type <- list(met = FALSE,
                        want = "list of dataframes",
                        is = "data.frame")
    } else if (is.list(x)) {
      data_type <- list(met = TRUE)
    } else {
      data_type <- list(met = FALSE,
                        want = "list of dataframes",
                        is = class(x))
    }
  } else {
    if(is.data.frame(x)){
      data_type <- list(met = TRUE)
    } else {
      data_type <- list(met = FALSE,
                        want = "list of dataframes",
                        is = class(x))
    }
  }
  data_type
}


validate_n_tables <- function(x, n_tables_expected){
  n_tables <- NULL
  if(n_tables_expected > 1){
    if(is.data.frame(x)){
      n_tables <- list(met = FALSE,
                              want = n_tables_expected,
                              is = 1)
    } else if (is.list(x)) {
      n_tables_actual <- length(x)
      met <- FALSE
      if(n_tables_actual == n_tables_expected | n_tables_actual > n_tables_expected) met <- TRUE
      n_tables <- list(met = TRUE)
      if(!met){
        n_tables <- list(met = met,
                                want = n_tables_expected,
                                is = n_tables_actual)
      }
    }
  } else {
    if(is.data.frame(x)){
      n_tables <- list(met = TRUE)
    }
  }
  n_tables
}

validate_table_ids <- function(x, n_tables_expected, table_ids_expected, data_type_met){
  table_ids <- NULL
  if(n_tables_expected > 1){
    table_ids_actual <- names(x)
    table_ids <- list(met = TRUE)

    if(!data_type_met){
      table_ids <- list(met = FALSE,
                        want = table_ids_expected,
                        is = "none")
    } else if(is.null(table_ids_actual)){
      table_ids <- list(met = FALSE,
                        want = table_ids_expected,
                        is = "unnamed tables")
    } else if (!all(table_ids_expected %in% table_ids_actual)){
      table_ids <- list(met = FALSE,
                        want = table_ids_expected,
                        is = table_ids_actual)
    }
  }
  table_ids
}
