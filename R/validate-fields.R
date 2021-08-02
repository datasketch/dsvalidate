validate_fields <- function(x, requirements){

  output <- list()

  table_ids <- names(requirements$table)

  for(table_id in table_ids){
    table <- x[[table_id]]
    dic <- create_dic(table, extended = TRUE)

    fields <- requirements$table[[table_id]]$fields
    field_ids <- names(fields)

    # If n_cols greater than 0 has been left out as short-cut it is added here
    fields <- fields %>% purrr::map(function(.x){
      if(is.null(.x$n_cols)) .x$n_cols <- list(greater_than = 0)
      if(is.null(.x$id_required)) .x$id_required <- FALSE
      .x
    })

    specs_checked <- field_ids %>% purrr::map(function(.x){

      field <- fields[[.x]]
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
        dplyr::mutate("met_{.x}" := all(dplyr::c_across(!id))) %>%
        dplyr::select(id, dplyr::starts_with("met_")) %>%
        dplyr::mutate("met_id_{.x}" := id == .x)

      use_ids <- NULL

      equals_required_id <- field_validated[[paste0("met_id_", .x)]]
      id_found <- any(equals_required_id)

      met <- FALSE
      is_n_cols <- 0
      use_cols <- NULL
      if(id_required){
        if(id_found){
          id_meets_requirements <- field_validated[equals_required_id,][[paste0("met_", .x)]]
          met <- id_meets_requirements
          is_n_cols <- 1
          use_cols <- .x
        }
      } else {
        specs_met <- field_validated[[paste0("met_", .x)]]
        is_n_cols <- sum(specs_met)

        sufficient_n_cols <- eval_conditions(is_n_cols, names(want_n_cols), want_n_cols[[1]])
        if(sufficient_n_cols){
          met <- TRUE
          use_cols <- field_validated[specs_met,]$id
        }

      }

      list(met = met,
           id_found = id_found,
           id_required = id_required,
           specs = specs,
           want_n_cols = want_n_cols,
           is_n_cols = is_n_cols,
           col_used_multiple = NULL,
           use_cols = use_cols)

    }) %>% purrr::set_names(field_ids)

    columns_required_by_multiple_specs <- specs_checked %>%
      purrr::map(`[`, "use_cols") %>%
      purrr::map(1) %>%
      purrr::reduce(intersect)

    output <- specs_checked
    if(!is.null(columns_required_by_multiple_specs)){
      output <- specs_checked %>% purrr::map(function(.x){
        col_used_multiple <- intersect(columns_required_by_multiple_specs, .x$use_cols)
        if(length(col_used_multiple) > 0) .x$output$met <- FALSE; .x$output$col_used_multiple <- col_used_multiple
        .x$output
      })
    }
  }

  output
}

