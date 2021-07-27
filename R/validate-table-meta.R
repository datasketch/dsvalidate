validate_table_meta <- function(x, requirements){

  output <- list()

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
