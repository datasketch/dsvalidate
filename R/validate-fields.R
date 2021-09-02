#' Validate fields
#'
#' For each table, validates all field specifications
#'
#' @param x Dataframe or list of dataframe
#' @param requirements List of requirements
#' @param validated_table_meta Boolean, whether or not table meta requirements were met
#' @param validated_table_specs Boolean, whether or not table specs requirements were met
#'
#' @return List of validations
validate_fields <- function(x, requirements, validated_table_meta = TRUE, validated_table_specs = TRUE){

  if(!validated_table_meta | !validated_table_specs) return(NULL)

  output <- list()

  table_ids <- names(requirements$table)

  for(table_id in table_ids){

    if(length(table_ids) == 1){
      table <- x
    } else {
      table <- x[[table_id]]
    }

    dic <- create_dic(table, extended = TRUE)

    fields <- check_fields(fields = requirements$table[[table_id]]$fields)

    field_ids <- names(fields)

    # Initialise variables
    unavailable_columns <- c()
    output_table <- list()

    field_requirements_checked <- fields %>%
      purrr::map(~check_field_requirements(field = .x, dic = dic)) %>%
      purrr::set_names(field_ids)

    fields_ordered <- order_fields(fields = field_requirements_checked)

    # Start with those field specification for which there are just enough columns that satisfy requirement.
    # Once columns are assigned to a field specification, they are marked as 'unavilable_columns' for the next field specification.
    # The more columns there are for a field specification to 'choose from' the later this field specification happens in this loop.
    for(field in fields_ordered){

      validated_columns <- field$validated_columns
      req_n_cols <- field$n_cols

      met <- FALSE
      n_columns_available <- 0
      use_cols <- NULL

      if(field$id_required){
        if(field$diff_want_is == 0){
          met <- field$id_meets_requirements
          n_columns_available <- 1
          use_cols <- field$field_id
        }
      } else {
        specs_met <- validated_columns$meets_requirement

        # check if columns already 'used' by other field spec
        cols_specs_met <- validated_columns[specs_met,]$id
        col_used_in_other_requirement <- intersect(cols_specs_met, unavailable_columns)

        if(length(col_used_in_other_requirement) == 0) {
          col_used_in_other_requirement <- NULL
          available_columns <- cols_specs_met
        } else {
          available_columns <- cols_specs_met[!cols_specs_met %in% col_used_in_other_requirement]
        }

        n_columns_available <- length(available_columns)

        sufficient_n_cols <- eval_conditions(n_columns_available, names(req_n_cols), req_n_cols[[1]])

        if(sufficient_n_cols){
          met <- TRUE
          use_cols <- available_columns[1:(req_n_cols[[1]]+1)]
        }
      }

      # Add use_cols to unavailable_columns so in next iteration, the columns used in this iteration will not be available
      unavailable_columns <- unique(c(unavailable_columns, use_cols))

      if(met){
        field_validation <- list(met = TRUE,
                                 use_cols = use_cols)
      } else {

        field_validation <- list(met = FALSE,
                                 id_found = field$id_found,
                                 id_required = field$id_required,
                                 specs = field$specs,
                                 req_n_cols = req_n_cols,
                                 n_columns_available = n_columns_available,
                                 use_cols = use_cols,
                                 col_used_in_other_requirement = col_used_in_other_requirement)
      }

      output_table[[field$field_id]] <- field_validation

    }

    output_table$all_requirements_met <- all(output_table %>% purrr::map(~.x$met) %>% unlist())
    output[[table_id]] <- output_table
  }

  requirements_met <- output %>% purrr::map_lgl(~.x$all_requirements_met) %>% unlist()
  output$all_requirements_met <- all(requirements_met)
  output
}


#' Validate columns
#'
#' For all field specifications, check for each column in dic whether or not the column meets the fields requirements.
#'
#' @param dic Extended dictionary, created by created_dic(df, extended = TRUE)
#' @param specs List of field specifications
#' @param field_id Field id
#'
#' @return Tibble with an 'id' column for the column id, a boolean 'meets_requirements' column indicating whether the column meets
#' the field specifications and a boolean 'matches_id' column indicating whether the column id is equal to the field id.
validate_columns <- function(dic, specs, field_id){
  dic_selected <- dic %>%
    dplyr::select(id, dplyr::any_of(names(specs)) & !label)

  dic_selected %>%
    dplyr::mutate(dplyr::across(!id,  ~ eval(., specs, dplyr::cur_column()))) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(meets_requirement = all(dplyr::c_across(!id))) %>%
    dplyr::select(id, meets_requirement) %>%
    dplyr::mutate(matches_id = id == field_id) %>%
    dplyr::ungroup()
}


#' Check field requirements
#'
#' Create list of validated field requirements (before checking against OTHER field requirements)
#'
#' @param field Field requirements
#' @param dic Extended dictionary
#'
#' @return List of checked field requirements
check_field_requirements <- function(field, dic){

  field_id <- field$field_id
  id_required <- field$id_required

  validated_columns <- validate_columns(dic = dic, specs = field$specs, field_id = field_id)

  equals_required_id <- validated_columns$matches_id
  id_found <- any(equals_required_id)

  id_meets_requirements <- validated_columns[equals_required_id,]$meets_requirement

  # Difference between number of columns that satisfy requirement and number of columns needed
  diff_want_is <- get_diff_want_is(id_required, id_found, id_meets_requirements, validated_columns, field)

  field$id_found <- id_found
  field$id_meets_requirements <- id_meets_requirements
  field$validated_columns <- validated_columns
  field$diff_want_is <- diff_want_is
  field
}


#' Diff want is
#'
#' This function returns an integer inticating the difference between how many columns of a certain type are required
#' VS how many columns of that type exist in the data (as validated by validated_columns).
#'
#' If diff_want_is = 0, there are as many columns of a certain type available as required.
#' If diff_want_is > 0, there are fewer columns of a certain type than required.
#' If diff_want_is < 0, there are more columns of a certain type than required.
#'
#' In the special case that id_required (if a column is required to have a certain column name/id),
#' then diff_want_is = 0 if that column id exists in the data and it meets the requirements,
#' otherwise diff_want_is = -1
#'
#' @param id_required Boolean, whether or not the data must have a column with the specific id/name
#' @param id_found Boolean, whether the specific id/name has been found in the data
#' @param id_meets_requirements If id_found == TRUE, whether id meets field requirements
#' @param validated_columns Tibble of validated columns
#' @param field List of field specifications
#'
#' @return Integer
get_diff_want_is <- function(id_required, id_found, id_meets_requirements, validated_columns, field){
  diff_want_is <- NULL
  if(id_required){
    diff_want_is <- -1
    if(id_found & id_meets_requirements){
      diff_want_is <- 0
    }
  } else {
    specs_met <- validated_columns$meets_requirement
    n_cols_meet_requirement <- sum(specs_met)

    diff_want_is <- n_cols_meet_requirement - get_min_n_cols(req_n_cols = field$n_cols)
  }
  diff_want_is
}


#' Order fields
#'
#' Order field specifications by diff_want_is.
#'
#' @param fields List of field specifications
#'
#' @return Ordered list of field specification
order_fields <- function(fields){
  order <- fields %>%
    purrr::map_df("diff_want_is") %>%
    tidyr::pivot_longer(cols = everything()) %>%
    dplyr::arrange(value) %>%
    dplyr::select(-value) %>%
    dplyr::pull(name)

  fields[order]
}


get_min_n_cols <- function(req_n_cols){
  n_cols <- req_n_cols[[1]]
  if(names(req_n_cols) == "greater_than") n_cols <- n_cols + 1
  n_cols
}
