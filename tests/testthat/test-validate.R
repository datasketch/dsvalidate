
test_that("fields validated", {

  path <- system.file("test_dsvalidate", "ex03-network", "dsvalidate", package = "dsvalidate")
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
                                                              is_n_cols = 0,
                                                              use_cols = NULL,
                                                              col_used_in_other_requirement = NULL))

  expect_equal(output_validate_fields[[table_id]]$description, list(met = TRUE,
                                                                    id_found = FALSE,
                                                                    id_required = FALSE,
                                                                    specs = checked_fields$description$specs,
                                                                    req_n_cols = list(greater_than = 0),
                                                                    is_n_cols = 2,
                                                                    use_cols = "b",
                                                                    col_used_in_other_requirement = NULL))

})

