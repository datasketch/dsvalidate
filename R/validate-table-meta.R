validate_table_meta <- function(x, requirements){

  output <- list()

  table_ids_expected <- names(requirements$table)
  n_tables_expected <- length(table_ids_expected)

  output$data_type <- validate_data_type(x = x,
                                         n_tables_expected = n_tables_expected)

  output$n_tables <- validate_n_tables(x = x,
                                       n_tables_expected = n_tables_expected,
                                       validated_data_type = output$data_type$met)

  output$table_ids <- validate_table_ids(x = x,
                                         n_tables_expected = n_tables_expected,
                                         table_ids_expected = table_ids_expected,
                                         validated_data_type = output$data_type$met,
                                         validated_n_tables = output$n_tables$met)

  output
}


validate_data_type <- function(x, n_tables_expected){
  if(n_tables_expected > 1){
    want <- "list of dataframes"
    if(is.data.frame(x)){
      data_type <- not_validated(want = want,
                                 is = "data.frame")
    } else if (is.list(x)) {
      is_type_dataframe <- x %>% purrr::map(~ is.data.frame(.x)) %>% unlist()
      if(all(is_type_dataframe)){
        data_type <- validated()
      } else {
        data_type <- not_validated(want = want,
                                   is = "list contains non-dataframes")
      }

    } else {
      data_type <- not_validated(want = want,
                                 is = class(x))
    }
  } else {
    want <- "data.frame"
    if(is.data.frame(x)){
      data_type <- validated()
    } else if (is.list(x) & is.data.frame(x[[1]])){
      data_type <- validated()
    } else if (is.list(x)){
      data_type <- not_validated(want = want,
                                 is = paste0("list of ", class(x[[1]])))
    } else {
      data_type <- not_validated(want = want,
                                 is = class(x))
    }
  }
  data_type
}


validate_n_tables <- function(x, n_tables_expected, validated_data_type = TRUE){
  n_tables <- NULL

  if(!validated_data_type) return(not_validated(want = n_tables_expected,
                                                is = "none"))

  if(n_tables_expected > 1){
    if(is.data.frame(x)){
      n_tables <- not_validated(want = n_tables_expected,
                                is = 1)
    } else if (is.list(x)) {
      n_tables_actual <- length(x)
      met <- FALSE
      if(n_tables_actual == n_tables_expected | n_tables_actual > n_tables_expected) met <- TRUE
      n_tables <- validated()
      if(!met){
        n_tables <- not_validated(want = n_tables_expected,
                                  is = n_tables_actual)
      }
    }
  } else {
    if(is.data.frame(x)){
      n_tables <- validated()
    } else if(is.list(x)){
      if(length(x) == 1){
        n_tables <- validated()
      } else {
        n_tables <- not_validated(want = 1,
                                  is = length(x))
      }
    }
  }

  n_tables
}

validate_table_ids <- function(x, n_tables_expected, table_ids_expected, validated_data_type = TRUE, validated_n_tables = TRUE){
  table_ids <- NULL
  if(n_tables_expected > 1){

    if(!validated_data_type | !validated_n_tables) return(not_validated(want = table_ids_expected,
                                                                        is = "none"))

    table_ids_actual <- names(x)
    table_ids <- validated()

    if(is.null(table_ids_actual)){
      table_ids <- not_validated(want = table_ids_expected,
                                 is = "unnamed tables")
    } else if (!all(table_ids_expected %in% table_ids_actual)){
      table_ids <- not_validated(want = table_ids_expected,
                                 is = table_ids_actual)
    }


  }
  table_ids
}
