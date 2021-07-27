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

  # validate number of tables
  if(n_tables_expected > 1){
    if(is.data.frame(x)){
      output$data_type <- list(met = FALSE,
                               want = "list of dataframes",
                               is = "data.frame")

      output$n_tables <- list(met = FALSE,
                              want = n_tables_expected,
                              is = 1)
    } else if (is.list(x)) {
      output$data_type <- list(met = TRUE)
      n_tables <- length(x)
      met <- FALSE
      if(n_tables == n_tables_expected | n_tables > n_tables_expected) met <- TRUE

      output$n_tables <- list(met = TRUE)
      if(!met){
        output$n_tables <- list(met = met,
                                want = n_tables_expected,
                                is = n_tables)
      }
    } else {
      output$data_type <- list(met = FALSE,
                               want = "list of dataframes",
                               is = class(x))
    }

  } else {
    if(is.data.frame(x)){
      output$data_type <- list(met = TRUE)

      output$n_tables <- list(met = TRUE)
    } else {
      output$data_type <- list(met = FALSE,
                               want = "list of dataframes",
                               is = class(x))
    }
  }

  # validate table ids
  if(n_tables_expected > 1){
    table_ids <- names(x)
    output$table_ids <- list(met = TRUE)

    if(!output$data_type$met){
      output$table_ids <- list(met = FALSE,
                               want = table_ids_expected,
                               is = "none")
    } else if(is.null(table_ids)){
      output$table_ids <- list(met = FALSE,
                               want = table_ids_expected,
                               is = "unnamed tables")
    } else if (!all(table_ids_expected %in% table_ids)){
      output$table_ids <- list(met = FALSE,
                               want = table_ids_expected,
                               is = table_ids)
    }


  }

  output
}




