test_that("table meta validated", {

  # load requirements
  folder <- system.file("test_dsvalidate", "ex03-network", "dsvalidate", package = "dsvalidate")
  file_path <- file.path(folder,"requirements.yaml")
  requirements <- yaml::read_yaml(file_path)

  # Idea: validate function accepts a single dataframe or a list of dataframes. If there are multiple tables specified
  # in the requirements.yaml, validate needs to be passed a list of dataframes, otherwise it should raise an error.

  df <- data.frame(col = c(LETTERS[1:19]))
  df2 <- data.frame(col1 = c(1:20),
                    col2 = c(rep("A", 10), rep("B", 10)))

  l_df <- list(nodes = df, edges = df2)
  l_df2 <- list(nodes = df, somethingelse = df2)


  output_insufficient_tables <- validate_table_meta(x = df,
                                                    requirements = requirements)

  output_enough_tables <- validate_table_meta(x = l_df,
                                              requirements = requirements)

  output_wrong_ids <- validate_table_meta(x = l_df2,
                                          requirements = requirements)

  expect_equal(output_insufficient_tables$n_tables, list(met = FALSE,
                                                         want = 2,
                                                         is = 1))

  expect_equal(output_insufficient_tables$table_ids, list(met = FALSE,
                                                          want = names(requirements$table),
                                                          is = "none"))

  expect_equal(output_wrong_ids$table_ids, list(met = FALSE,
                                                want = names(requirements$table),
                                                is = c("nodes", "somethingelse")))

  expect_equal(output_enough_tables$n_tables, list(met = TRUE))
  expect_equal(output_enough_tables$table_ids, list(met = TRUE))

})


test_that("table specs validated", {

  # load requirements
  folder <- system.file("test_dsvalidate", "ex03-network", "dsvalidate", package = "dsvalidate")
  file_path <- file.path(folder,"requirements.yaml")
  requirements <- yaml::read_yaml(file_path)

  df <- data.frame(col = c())
  df1 <- data.frame(col1 = c(1:20),
                    col2 = c(rep("A", 10), rep("B", 10)))

  l_df <- list(nodes = df,
               edges = df1)

  table_id <- "nodes"

  output_too_few_columns <- validate_table_specs(x = l_df,
                                                 requirements = requirements)

  expect_equal(output_too_few_columns[[table_id]]$specs, list(met = FALSE,
                                                              items = 3,
                                                              passes = 0,
                                                              fails = 3))

  expect_equal(output_too_few_columns[[table_id]]$n_cols, list(met = FALSE,
                                                               want = requirements$table[[table_id]]$specs$n_cols,
                                                               is = 0))

  expect_equal(output_too_few_columns[[table_id]]$n_rows, list(met = FALSE,
                                                               want = requirements$table[[table_id]]$specs$n_rows,
                                                               is = 0))

  expect_equal(output_too_few_columns[[table_id]]$frType, list(met = FALSE,
                                                               want = requirements$table[[table_id]]$specs$frType,
                                                               is = ""))

})


test_that("fields validated", {

  # load requirements
  folder <- system.file("test_dsvalidate", "ex03-network", "dsvalidate", package = "dsvalidate")
  file_path <- file.path(folder,"requirements.yaml")
  requirements <- yaml::read_yaml(file_path)

  df <- data.frame(id = c(1:20),
                   b = c(rep("A", 10), rep("B", 10)),
                   c = c(rep("A", 10), rep("B", 10)))
  f <- homodatum::fringe(df)
  df1 <- data.frame(col1 = c(rep("A", 5), rep("B", 5), rep("C", 10)),
                    col2 = c(rep("A", 10), rep("B", 10)))
  x <- list(nodes = df,
            edges = df1)

  table_id <- "nodes"

  output_check_fields <- validate_fields(x = x,
                                         requirements = requirements)

  expect_equal(output_check_fields[[table_id]]$specs$frType, list(met = TRUE))


  expect_equal(output_check_fields[[table_id]]$fields, list(met = FALSE,
                                                            items = 3,
                                                            passes = 2,
                                                            fails = 1))

  expect_equal(output_check_fields[[table_id]]$id$met, TRUE)

  expect_equal(output_check_fields[[table_id]]$label, list(met = FALSE,
                                                           id_found = FALSE,
                                                           id_required = FALSE,
                                                           specs = requirements$table[[table_id]]$fields$label$specs,
                                                           want_n_cols = list(greater_than = 0),
                                                           is_n_cols = 0,
                                                           use_cols = NULL,
                                                           col_used_in_other_requirement = NULL))

  expect_equal(output_check_fields[[table_id]]$description, list(met = TRUE,
                                                                 id_found = FALSE,
                                                                 id_required = FALSE,
                                                                 specs = requirements$table[[table_id]]$fields$description$specs,
                                                                 want_n_cols = list(greater_than = 0),
                                                                 is_n_cols = 2,
                                                                 use_cols = "b",
                                                                 col_used_in_other_requirement = NULL))

})

