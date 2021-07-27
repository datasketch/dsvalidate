validate_fields <- function(x, requirements){

  output <- list()

  table_ids <- names(requirements$table)

  for(table_id in table_ids){
    table <- x[[table_id]]
    dic <- create_dic(table, extended = TRUE)

    fields <- requirements$table[[table_id]]$fields
    field_ids <- names(fields)

    for(field_id in field_ids)
    specs <- fields[[field_id]]$specs

    # TODO same for all specs
    if(!is.null(specs$hdType)) specs$hdType <- list(equals = specs$hdType)

    dic_selected <- dic %>% dplyr::select(dplyr::any_of(names(specs)))
  }

  output
}
