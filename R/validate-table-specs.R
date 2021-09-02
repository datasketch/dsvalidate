validate_table_specs <- function(x, requirements, validated_table_meta = TRUE){

  if(!validated_table_meta) return(NULL)

  output <- list()

  table_ids <- names(requirements$table)

  for(table_id in table_ids){
    specs <- requirements$table[[table_id]]$specs
    table <- x[[table_id]]

    output[[table_id]]$n_cols <- validate_n_cols(table = table, specs_n_cols = specs$n_cols)

    output[[table_id]]$n_rows <- validate_n_rows(table = table, specs_n_rows = specs$n_rows,
                                                 validated_n_cols = output[[table_id]]$n_cols$met)

    output[[table_id]]$frType <- validate_frType(table = table, specs_frType = specs$frType,
                                                 validated_n_cols = output[[table_id]]$n_cols$met,
                                                 validated_n_rows = output[[table_id]]$n_rows$met)

    output[[table_id]]$all_table_requirements_met <- all(output[[table_id]]$n_cols$met,
                                                         output[[table_id]]$n_rows$met,
                                                         output[[table_id]]$frType$met)
  }

  requirements_met <- output %>% purrr::map_lgl(~.x$all_table_requirements_met) %>% unlist()

  output$all_requirements_met <- all(requirements_met)
  output
}


validate_n_cols <- function(table, specs_n_cols){

  n_cols_actual <- ncol(table)

  met <- eval_conditions(n_cols_actual, names(specs_n_cols), specs_n_cols[[1]])

  n_cols <- validated()

  if(!met){
    n_cols <- not_validated(want = specs_n_cols,
                            is = n_cols_actual)
  }

  n_cols
}


validate_n_rows <- function(table, specs_n_rows, validated_n_cols = TRUE){

  if(!validated_n_cols) return(not_validated(want = specs_n_rows,
                                             is = 0))

  n_rows_actual <- nrow(table)

  met <- eval_conditions(n_rows_actual, names(specs_n_rows), specs_n_rows[[1]])

  n_rows <- validated()

  if(!met){
    n_rows <- not_validated(want = specs_n_rows,
                            is = n_rows_actual)
  }

  n_rows
}


validate_frType <- function(table, specs_frType, validated_n_cols = TRUE, validated_n_rows = TRUE){

  if(!validated_n_cols | !validated_n_rows) return(not_validated(want = specs_frType,
                                                                 is = "none"))

  fringe_table <- homodatum::fringe(table)
  frType_actual_string <- as.character(fringe_table$frtype)
  frType_actual <- unlist(strsplit(frType_actual_string, split='-', fixed=TRUE))

  condition <- names(specs_frType)
  frType_expected <- unlist(strsplit(specs_frType[[condition]], split='-', fixed=TRUE))

  met <- eval_conditions(frType_actual, condition, frType_expected)

  frType <- validated()

  if(!met){
    frType <- not_validated(want = specs_frType,
                            is = frType_actual_string)
  }

  frType
}

