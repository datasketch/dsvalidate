#' Load requirements for validation
#'
#' Loads and checks the requirements.yaml file, raises necessary
#' errors and warnings if necessary, and prepares it for use by
#' validate_requirements.
#'
#' @param path Path for the requirements.yaml file; default is 'dsvalidate'
#' @param filename Filename of requirements file, default is 'requirements.yaml'
#'
#' @return List of requirements
#' @export
requirements_load <- function(path = "dsvalidate", filename = "requirements.yaml"){

  if(!dir.exists(path)) stop("dsvalidate folder not found")

  if(!grepl(".yaml", filename)) stop("Requirements file needs to be of type .yaml.")

  file_path <- file.path(path, filename)
  file_exists <- file.exists(file_path)

  if(!file_exists) stop("Requirements file not found.")

  requirements <- yaml::read_yaml(file_path)

  if(length(requirements) == 0) stop("Requirements file empty.")

  table_specs_exist <- "table" %in% names(requirements)
  if(!table_specs_exist) stop("Requirements file doesn't contain table specs.")

  table_names <- names(requirements$table)

  table_specs_non_empty <- length(table_names) > 0
  if(!table_specs_non_empty) stop("Requirements file doesn't contain table specs.")

  table_names %>% purrr::map(function(.x){
    if(any(!c("table_id", "specs", "fields") %in% names(requirements$table[[.x]]))) stop("All table requirements must include table_id, specs, and fields.")
  })

  requirements$table <- requirements$table %>% purrr::map(function(.x){
    if(any(!c("table_id", "specs", "fields") %in% names(.x))) stop("All table requirements must include table_id, specs, and fields.")
    if(!"n_cols" %in% names(.x$specs)) .x$specs$n_cols <- list(greater_than = 0)
    if(!"n_rows" %in% names(.x$specs)) .x$specs$n_rows <- list(greater_than = 0)
    .x
  })

  requirements
}
