validate_table_specs <- function(x, requirements){

  output <- list()

  table_ids <- names(requirements$table)

  for(table_id in table_ids){
    specs <- requirements$table[[table_id]]$specs
    table <- x[[table_id]]

    output[[table_id]]$n_cols <- validate_n_cols(table = table, specs_n_cols = specs$n_cols)

    output[[table_id]]$n_rows <- validate_n_rows(table = table, specs_n_rows = specs$n_rows)

    output[[table_id]]$frType <- validate_frType(table = table, specs_frType = specs$frType)
  }

  output
}


validate_frType <- function(table, specs_frType){
  if(is.null(specs_frType)) return()

  fringe_table <- homodatum::fringe(table)
  frType_actual_string <- as.character(fringe_table$frtype)
  frType_actual <- unlist(strsplit(frType_actual_string, split='-', fixed=TRUE))

  condition <- names(specs_frType)
  frType_expected <- unlist(strsplit(specs_frType[[condition]], split='-', fixed=TRUE))

  met <- eval_conditions(frType_expected, condition, frType_actual)

  frType <- list(met = TRUE)

  if(!met){
    frType <- list(met = FALSE,
                   want = specs_frType,
                   is = frType_actual_string)
  }

  frType
}



validate_n_cols <- function(table, specs_n_cols){
  if(is.null(specs_n_cols)) return()

  n_cols_actual <- ncol(table)

  met <- eval_conditions(n_cols_actual, names(specs_n_cols), specs_n_cols[[1]])

  n_cols <- list(met = TRUE)

  if(!met){
    n_cols <- list(met = FALSE,
                   want = specs_n_cols,
                   is = n_cols_actual)
  }

  n_cols
}


validate_n_rows <- function(table, specs_n_rows){
  if(is.null(specs_n_rows)) return()

  n_rows_actual <- nrow(table)

  met <- eval_conditions(n_rows_actual, names(specs_n_rows), specs_n_rows[[1]])

  n_rows <- list(met = TRUE)

  if(!met){
    n_rows <- list(met = FALSE,
                   want = specs_n_rows,
                   is = n_rows_actual)
  }

  n_rows
}



