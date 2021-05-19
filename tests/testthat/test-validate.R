test_that("requirements validated", {

  # load requirements
  folder <- system.file("test_dsvalidate", "ex02-two-categorical-cols-max-distinct-cat", package = "dsvalidate")
  file_path <- file.path(path,"requirements.yaml")
  requirements <- yaml::read_yaml(file_path)

  df_fewer_columns <- data.frame(col1 = c(LETTERS[1:19]))

  df_num_column <- data.frame(col1 = c(1:20),
                              col2 = c(rep("A", 10), rep("B", 10)))

  df_n_distinct_20 <- data.frame(col1 = c(LETTERS[1:20]),
                                 col2 = c(rep("A", 10), rep("B", 10)))

  df_n_distinct_19 <- data.frame(col1 = c(LETTERS[1:19]),
                                 col2 = c(rep("A", 10), rep("B", 9)))

  output_fewer_columns <- validate_requirements(df = df_fewer_columns,
                                                requirements = requirements)

  output_num_column <- validate_requirements(df = df_num_column,
                                             requirements = requirements)

  output_distinct_20 <- validate_requirements(df = df_n_distinct_20,
                                              requirements = requirements)

  output_distinct_19 <- validate_requirements(df = df_n_distinct_19,
                                              requirements = requirements)

  # test label output
  expect_equal(output_fewer_columns$label, "cat_cols_fewer_than_20_distinct")

  # test overall requirements output
  expect_false(output_fewer_columns$requirements)
  expect_false(output_num_column$requirements)
  expect_false(output_distinct_20$requirements)
  expect_true(output_distinct_19$requirements)

  # test individual conditions
  expect_false(output_fewer_columns$conditions$n_cols_greater_than_1)
  expect_true(output_num_column$conditions$n_cols_greater_than_1)
  expect_true(output_distinct_20$conditions$n_cols_greater_than_1)
  expect_true(output_distinct_19$conditions$n_cols_greater_than_1)

  expect_null(output_fewer_columns$conditions$hdType_equals_Cat)
  expect_false(output_num_column$conditions$hdType_equals_Cat)
  expect_true(output_distinct_20$conditions$hdType_equals_Cat)
  expect_true(output_distinct_19$conditions$hdType_equals_Cat)

  expect_null(output_fewer_columns$conditions$n_distinct_less_than_20)
  expect_null(output_num_column$conditions$n_distinct_less_than_20)
  expect_false(output_distinct_20$conditions$n_distinct_less_than_20)
  expect_true(output_distinct_19$conditions$n_distinct_less_than_20)


  ### the output of validate_requirements would look as follows (not part of test, just here for clarification)
  expected_output_fewer_columns <- list(label = "cat_cols_fewer_than_20_distinct",
                                        requirements = FALSE,
                                        conditions = list(n_cols_greater_than_1 = FALSE,
                                                          # thought of NULL for hdType and n_distinct requirement: if n_cols condition is not satisfied
                                                          # the validator stops validating remaining requirements
                                                          hdType_equals_Cat = NULL,
                                                          n_distinct_less_than_20 = NULL))

  expected_outpout_num_column <- list(label = "cat_cols_fewer_than_20_distinct",
                                      requirements = FALSE,
                                      conditions = list(n_cols_greater_than_1 = TRUE,
                                                        hdType_equals_Cat = FALSE,
                                                        # thought of NULL for n_distinct requirement: if hdType condition is not satisfied
                                                        # the validator stops validating remaining requirements
                                                        n_distinct_less_than_20 = NULL))

  expected_output_distinct_20 <- list(label = "cat_cols_fewer_than_20_distinct",
                                      requirements = FALSE,
                                      conditions = list(n_cols_greater_than_1 = TRUE,
                                                        hdType_equals_Cat = TRUE,
                                                        n_distinct_less_than_20 = FALSE))

  expected_output_distinct_19 <- list(label = "cat_cols_fewer_than_20_distinct",
                                      requirements = TRUE,
                                      conditions = list(n_cols_greater_than_1 = TRUE,
                                                        hdType_equals_Cat = TRUE,
                                                        n_distinct_less_than_20 = TRUE))

})
