test_that("columns validated", {
  df <- data.frame(id = c(1:20),
                   b = c(rep("A", 10), rep("B", 10)),
                   c = c(rep("A", 10), rep("B", 10)))
  f <- homodatum::fringe(df)
  dic <- create_dic(df, extended = TRUE)

  specs <- list(hdType = list(is_any_of = c("Cat", "Num")),
                unique = list(equals = TRUE),
                n_distinct = list(greater_than = 10))

  expected_output <- dplyr::tibble(id = names(df),
                                   meets_requirement = c(TRUE, FALSE, FALSE),
                                   matches_id = c(FALSE, TRUE, FALSE))

  actual_output <- validate_columns(dic, specs, field_id = "b")

  expect_equal(actual_output, expected_output)

})


test_that("field requirements checked", {

  specs <- list(hdType = list(is_any_of = c("Cat", "Num")),
                unique = list(equals = TRUE),
                n_distinct = list(greater_than = 10))

  field <- list(field_id = "id",
                label = "ID of a node",
                n_cols = list(greater_than = 0),
                id_required = TRUE,
                specs = specs)

  df <- data.frame(id = c(1:20),
                   b = c(rep("A", 10), rep("B", 10)),
                   c = c(rep("A", 10), rep("B", 10)))
  f <- homodatum::fringe(df)
  dic <- create_dic(df, extended = TRUE)


  expected_output <- field
  expected_output$id_found <- TRUE
  expected_output$id_meets_requirements <- TRUE
  expected_output$validated_columns <- validate_columns(dic, specs, field_id = "id")
  expected_output$diff_want_is <- 0

  actual_output <- check_field_requirements(field, dic)

  expect_equal(actual_output, expected_output)

})


test_that("fields validated", {

  path <- system.file("test_dsvalidate", "ex02-network", "dsvalidate", package = "dsvalidate")
  requirements <- requirements_load(path = path)

  df <- data.frame(id = c(1:20),
                   b = c(rep("A", 10), rep("B", 10)),
                   c = c(rep("A", 10), rep("B", 10)))
  f <- homodatum::fringe(df)
  df1 <- data.frame(col1 = c(rep("A", 5), rep("B", 5), rep("C", 10)),
                    col2 = c(rep("A", 10), rep("B", 10)))
  x <- list(nodes = df,
            edges = df1)

  table_id <- "nodes"

  output_validate_fields <- validate_fields(x = x,
                                            requirements = requirements)


  checked_fields <- check_fields(requirements$table[[table_id]]$fields)


  expect_equal(output_validate_fields[[table_id]]$id$met, TRUE)

  expect_equal(output_validate_fields[[table_id]]$label, list(met = FALSE,
                                                              id_found = FALSE,
                                                              id_required = FALSE,
                                                              specs = checked_fields$label$specs,
                                                              req_n_cols = list(greater_than = 0),
                                                              n_columns_available = 0,
                                                              use_cols = NULL,
                                                              col_used_in_other_requirement = NULL))

  expect_equal(output_validate_fields[[table_id]]$description, list(met = TRUE,
                                                                    use_cols = "b"))


  table_id <- "edges"

  expect_equal(output_validate_fields[[table_id]]$target, list(met = TRUE,
                                                               use_cols = "col1"))

  expect_equal(output_validate_fields[[table_id]]$source, list(met = TRUE,
                                                               use_cols = "col2"))


  expect_null(validate_fields(x, requirements, validated_table_meta = FALSE))
  expect_null(validate_fields(x, requirements, validated_table_specs = FALSE))

})

