validate_fields <- function(x, requirements){

  output <- list()

  table_ids <- names(requirements$table)

  for(table_id in table_ids){
    table <- x[[table_id]]
    dic <- create_dic(table, extended = TRUE)

    fields <- requirements$table[[table_id]]$fields

    # If n_cols greater than 0 has been left out as short-cut it is added here
    fields <- fields %>% purrr::map(function(.x){
      if(is.null(.x$n_cols)) .x$n_cols <- list(greater_than = 0)
      if(is.null(.x$id_required)) .x$id_required <- FALSE
      .x
    })

    # Order field specs so that loop starts with those that need matching field id
    field_ids_ordered <- fields %>%
      purrr::map_df("id_required") %>%
      tidyr::pivot_longer(cols = everything()) %>%
      dplyr::arrange(desc(value)) %>%
      dplyr::select(-value) %>%
      dplyr::pull(name)

    columns_used <- c()

    output_table <- list()

    for(field_id in field_ids_ordered){

      field <- fields[[field_id]]
      id_required <- field$id_required
      want_n_cols <- field$n_cols
      if(!is.list(want_n_cols)) want_n_cols <- list(equals = want_n_cols)

      specs <- field$specs

      # If `equals` has been left out as short-cut it is added here
      for(specs_type in names(specs)){
        specification <- specs[[specs_type]]
        if(!is.list(specification)) specs[[specs_type]] <- list(equals = specification)
      }

      dic_selected <- dic %>% dplyr::select(id, dplyr::any_of(names(specs)) & !label)

      field_validated <- dic_selected %>%
        dplyr::mutate(dplyr::across(!id,  ~ eval(., specs, dplyr::cur_column()))) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(meets_requirement = all(dplyr::c_across(!id))) %>%
        dplyr::select(id, meets_requirement) %>%
        dplyr::mutate(matches_id = id == field_id)

      use_cols <- NULL
      col_used_in_other_requirement <- NULL

      equals_required_id <- field_validated$matches_id
      id_found <- any(equals_required_id)

      met <- FALSE
      is_n_cols <- 0
      use_cols <- NULL
      if(id_required){
        if(id_found){
          id_meets_requirements <- field_validated[equals_required_id,]$meets_requirement
          met <- id_meets_requirements
          is_n_cols <- 1
          use_cols <- field_id
        }
      } else {
        specs_met <- field_validated$meets_requirement
        cols_specs_met <- field_validated[specs_met,]$id
        col_used_in_other_requirement <- intersect(cols_specs_met, columns_used)
        col_not_used_in_other_requirement <- cols_specs_met[!cols_specs_met %in% col_used_in_other_requirement]

        is_n_cols <- length(col_not_used_in_other_requirement)

        sufficient_n_cols <- eval_conditions(is_n_cols, names(want_n_cols), want_n_cols[[1]])
        if(sufficient_n_cols){
          met <- TRUE
          use_cols <- field_validated[specs_met,]$id[1:(want_n_cols[[1]]+1)]
          col_used_in_other_requirement <- NULL
        } else {
          if(length(col_used_in_other_requirement) == 0) {
            col_used_in_other_requirement <- NULL
          }
        }
      }

      columns_used <- unique(c(columns_used, use_cols))

      output_table[[field_id]] <- list(met = met,
                                       id_found = id_found,
                                       id_required = id_required,
                                       specs = specs,
                                       want_n_cols = want_n_cols,
                                       is_n_cols = is_n_cols,
                                       use_cols = use_cols,
                                       col_used_in_other_requirement = col_used_in_other_requirement)

    }

    output[[table_id]] <- output_table
  }

  output
}

