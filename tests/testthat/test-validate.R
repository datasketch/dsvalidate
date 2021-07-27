test_that("multiple table requirements validated", {

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


  output_insufficient_tables <- validate_requirements(df = df,
                                                      requirements = requirements)

  output_enough_tables <- validate_requirements(df = l_df,
                                                requirements = requirements)

  output_wrong_ids <- validate_requirements(df = l_df2,
                                            requirements = requirements)

  expect_equal(output_insufficient_tables$table$n_tables, list(met = FALSE,
                                                               want = 2,
                                                               is = 1))

  expect_equal(output_insufficient_tables$table$table_ids, list(met = FALSE,
                                                                want = names(requirements$table),
                                                                is = NULL))

  expect_equal(output_wrong_ids$table$table_ids, list(met = FALSE,
                                                      want = names(requirements$table),
                                                      is = c("nodes", "somethingelse")))

  expect_equal(output_enough_tables$table$n_tables, list(met = TRUE))
  expect_equal(output_enough_tables$table$table_ids, list(met = TRUE))

})


test_that("table specifications validated", {

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

  output_too_few_columns <- validate_requirements(df = l_df,
                                                  requirements = requirements)

  expect_equal(output_too_few_columns[[table_id]]$specs, list(met = FALSE,
                                                              items = 3,
                                                              passes = 0,
                                                              fails = 3))

  expect_equal(output_too_few_columns[[table_id]]$specs$n_cols, list(met = FALSE,
                                                                     want = requirements$table[[table_id]]$specs$n_cols,
                                                                     is = 0))

  expect_equal(output_too_few_columns[[table_id]]$specs$n_rows, list(met = FALSE,
                                                                     want = requirements$table[[table_id]]$specs$n_rows,
                                                                     is = 0))

  expect_equal(output_too_few_columns[[table_id]]$specs$frType, list(met = FALSE,
                                                                     want = requirements$table[[table_id]]$specs$frType,
                                                                     is = frType("")))

})


test_that("field requirements validated", {

  # load requirements
  folder <- system.file("test_dsvalidate", "ex03-network", "dsvalidate", package = "dsvalidate")
  file_path <- file.path(folder,"requirements.yaml")
  requirements <- yaml::read_yaml(file_path)

  df <- data.frame(a = c(1:20),
                   b = c(rep("A", 10), rep("B", 10)),
                   c = c(rep("A", 10), rep("B", 10)))
  f <- homodatum::fringe(df)
  df1 <- data.frame(col1 = c(1:20),
                    col2 = c(rep("A", 10), rep("B", 10)))
  l_df <- list(nodes = df,
               edges = df1)

  table_id <- "nodes"

  output_check_fields <- validate_requirements(df = l_df,
                                               requirements = requirements)

  expect_equal(output_check_fields[[table_id]]$specs$frType, list(met = TRUE))


  expect_equal(output_check_fields[[table_id]]$fields, list(met = FALSE,
                                                            items = 3,
                                                            passes = 2,
                                                            fails = 1))

  expect_equal(output_check_fields[[table_id]]$fields$id, list(met = TRUE))

  expect_equal(output_check_fields[[table_id]]$fields$label, list(met = FALSE,
                                                                  req = requirements$table[[table_id]]$fields$label,
                                                                  want = list(greater_than = 0),
                                                                  is = 0))

  expect_equal(output_check_fields[[table_id]]$fields$description, list(met = TRUE))

})

