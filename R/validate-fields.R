validate_fields <- function(x, requirements){

  output <- list()

  table_ids <- names(requirements$table)

  for(table_id in table_ids){
    table <- x[[table_id]]
    dic <- create_dic(table, extended = TRUE)

    fields <- check_fields(fields = requirements$table[[table_id]]$fields)

    field_ids <- names(fields)

    # Initialise variables
    columns_used <- c()
    output_table <- list()

    fields_checked <- fields %>% purrr::map(function(.x){

      field_id <- .x$field_id
      id_required <- .x$id_required

      validated_columns <- validate_columns(dic = dic, specs = .x$specs, field_id = field_id)

      equals_required_id <- validated_columns$matches_id
      id_found <- any(equals_required_id)

      id_meets_requirements <- NULL

      # Difference between number of columns that satisfy requirement and number of columns needed
      diff_want_is <- NULL
      if(id_required){
        diff_want_is <- -1
        if(id_found){
          id_meets_requirements <- validated_columns[equals_required_id,]$meets_requirement
          if(id_meets_requirements){
            diff_want_is <- 0
          }
        }
      } else {
        specs_met <- validated_columns$meets_requirement
        n_cols_meet_requirement <- sum(specs_met)

        diff_want_is <- n_cols_meet_requirement - get_min_n_cols(req_n_cols = .x$n_cols)
      }

      .x$id_found <- id_found
      .x$id_meets_requirements <- id_meets_requirements
      .x$validated_columns <- validated_columns
      .x$req_n_cols <- .x$n_cols
      .x$diff_want_is <- diff_want_is
      .x
    }) %>% purrr::set_names(field_ids)


    # Order field specs so that loop starts with those where just enough columns satisfy requirement
    fields_ordered <- order_fields(fields = fields_checked)

    for(field in fields_ordered){

      validated_columns <- field$validated_columns
      req_n_cols <- field$req_n_cols

      met <- FALSE
      is_n_cols <- 0
      use_cols <- NULL

      if(field$id_required){
        if(field$diff_want_is == 0){
          met <- field$id_meets_requirements
          is_n_cols <- 1
          use_cols <- field$field_id
        }
      } else {
        specs_met <- validated_columns$meets_requirement
        cols_specs_met <- validated_columns[specs_met,]$id

        # check if columns already 'used' by other field spec
        col_used_in_other_requirement <- intersect(cols_specs_met, columns_used)
        col_not_used_in_other_requirement <- cols_specs_met[!cols_specs_met %in% col_used_in_other_requirement]

        is_n_cols <- length(col_not_used_in_other_requirement)

        sufficient_n_cols <- eval_conditions(is_n_cols, names(req_n_cols), req_n_cols[[1]])
        if(sufficient_n_cols){
          met <- TRUE
          use_cols <- validated_columns[specs_met,]$id[1:(req_n_cols[[1]]+1)]
          col_used_in_other_requirement <- NULL
        } else {
          if(length(col_used_in_other_requirement) == 0) {
            col_used_in_other_requirement <- NULL
          }
        }
      }

      columns_used <- unique(c(columns_used, use_cols))

      output_table[[field$field_id]] <- list(met = met,
                                             id_found = field$id_found,
                                             id_required = field$id_required,
                                             specs = field$specs,
                                             req_n_cols = req_n_cols,
                                             is_n_cols = is_n_cols,
                                             use_cols = use_cols,
                                             col_used_in_other_requirement = col_used_in_other_requirement)

    }

    output[[table_id]] <- output_table
  }

  output
}


check_fields <- function(fields){
  fields %>% purrr::map(function(.x){
    .x$n_cols <- check_req_n_cols(n_cols = .x$n_cols)
    .x$specs <- check_specs(specs = .x$specs)
    if(is.null(.x$id_required)) .x$id_required <- FALSE
    .x
  })
}


check_specs <- function(specs){
  if(is.null(specs)) stop("Column specs missing.")
  # If `equals` has been left out as short-cut it is added here
  for(specs_type in names(specs)){
    specification <- specs[[specs_type]]
    if(!is.list(specification)) specs[[specs_type]] <- list(equals = specification)
  }

  specs
}


check_req_n_cols <- function(n_cols){
  # If n_cols is null, must be greater than 0
  if(is.null(n_cols)) n_cols <- list(greater_than = 0)
  # If n_cols greater than 0 has been left out as short-cut it is added here
  if(!is.list(n_cols)) n_cols <- list(equals = n_cols)
  n_cols
}


get_min_n_cols <- function(req_n_cols){
  n_cols <- req_n_cols[[1]]
  if(names(req_n_cols) == "greater_than") n_cols <- n_cols + 1
  n_cols
}


validate_columns <- function(dic, specs, field_id){
  dic_selected <- dic %>%
    dplyr::select(id, dplyr::any_of(names(specs)) & !label)

  dic_selected %>%
    dplyr::mutate(dplyr::across(!id,  ~ eval(., specs, dplyr::cur_column()))) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(meets_requirement = all(dplyr::c_across(!id))) %>%
    dplyr::select(id, meets_requirement) %>%
    dplyr::mutate(matches_id = id == field_id)
}


order_fields <- function(fields){
  order <- fields %>%
    purrr::map_df("diff_want_is") %>%
    tidyr::pivot_longer(cols = everything()) %>%
    dplyr::arrange(value) %>%
    dplyr::select(-value) %>%
    dplyr::pull(name)

  fields[order]
}
