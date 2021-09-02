test_that("number of columns validated", {

  df <- data.frame(col = c())
  df1 <- data.frame(col1 = c(1:20))
  df2 <- data.frame(col1 = c(rep("A", 10), rep("B", 10)),
                    col2 = c(rep("A", 10), rep("B", 10)))

  specs_n_cols_greater_than <- list(greater_than = 0)
  specs_n_cols_equals <- list(equals = 2)

  expect_equal(validate_n_cols(df, specs_n_cols = specs_n_cols_equals),
               not_validated(want = specs_n_cols_equals,
                             is = 0))

  expect_equal(validate_n_cols(df, specs_n_cols = specs_n_cols_greater_than),
               not_validated(want = specs_n_cols_greater_than,
                             is = 0))

  expect_equal(validate_n_cols(df1, specs_n_cols = specs_n_cols_equals),
               not_validated(want = specs_n_cols_equals,
                             is = 1))

  expect_equal(validate_n_cols(df1, specs_n_cols = specs_n_cols_greater_than),
               validated())


  expect_equal(validate_n_cols(df2, specs_n_cols = specs_n_cols_equals),
               validated())

  expect_equal(validate_n_cols(df2, specs_n_cols = specs_n_cols_greater_than),
               validated())

})


test_that("number of rows validated", {

  df <- data.frame(col = c())
  df1 <- data.frame(col1 = c(1:45))
  df2 <- data.frame(col1 = c(rep("A", 10), rep("B", 10)),
                    col2 = c(rep("A", 10), rep("B", 10)))

  specs_n_rows_greater_than0 <- list(greater_than = 0)
  specs_n_rows_greater_than50 <- list(greater_than = 50)
  specs_n_rows_less_than <- list(less_than = 30)
  specs_n_rows_equals <- list(equals = 20)

  expect_equal(validate_n_rows(df, specs_n_rows = specs_n_rows_greater_than0, validated_n_cols = FALSE),
               not_validated(want = specs_n_rows_greater_than0,
                             is = 0))

  expect_equal(validate_n_rows(df, specs_n_rows_greater_than0),
               not_validated(want = specs_n_rows_greater_than0,
                             is = nrow(df)))

  expect_equal(validate_n_rows(df1, specs_n_rows_greater_than50),
               not_validated(want = specs_n_rows_greater_than50,
                             is = nrow(df1)))

  expect_equal(validate_n_rows(df1, specs_n_rows_greater_than0),
               validated())

  expect_equal(validate_n_rows(df1, specs_n_rows_less_than),
               not_validated(want = specs_n_rows_less_than,
                             is = nrow(df1)))

  expect_equal(validate_n_rows(df, specs_n_rows_less_than),
               validated())

  expect_equal(validate_n_rows(df2, specs_n_rows_greater_than50),
               not_validated(want = specs_n_rows_greater_than50,
                             is = nrow(df2)))

  expect_equal(validate_n_rows(df2, specs_n_rows_equals),
               validated())

})


test_that("frType validated", {

  df <- data.frame(col = c())
  df1 <- data.frame(col1 = c(1:20),
                    col2 = c(rep("A", 10), rep("B", 10)))
  df2 <- data.frame(col1 = c(rep("A", 10), rep("B", 10)),
                    col2 = c(rep("A", 10), rep("B", 10)))
  df3 <- data.frame(col1 = c(1:20),
                    col2 = c(rep("A", 10), rep("B", 10)),
                    col3 = c(rep("A", 10), rep("B", 10)))

  specs_frType_contains <- list(contains_all = "Num-Cat-Cat")
  specs_frType_equals <- list(equals_all = "Cat-Cat")

  expect_equal(validate_frType(df, specs_frType = specs_frType_contains, validated_n_cols = FALSE),
               not_validated(want = specs_frType_contains,
                             is = "none"))

  expect_equal(validate_frType(df, specs_frType = specs_frType_contains, validated_n_rows = FALSE),
               not_validated(want = specs_frType_contains,
                             is = "none"))

  expect_equal(validate_frType(df1, specs_frType = specs_frType_contains),
               not_validated(want = specs_frType_contains,
                             is = "Num-Cat"))

  expect_equal(validate_frType(df3, specs_frType = specs_frType_contains),
               validated())

  expect_equal(validate_frType(df1, specs_frType = specs_frType_equals),
               not_validated(want = specs_frType_equals,
                             is = "Num-Cat"))

  expect_equal(validate_frType(df2, specs_frType = specs_frType_equals),
               validated())

})


test_that("table specs validated", {

  path <- system.file("test_dsvalidate", "ex02-network", "dsvalidate", package = "dsvalidate")
  requirements <- requirements_load(path = path)

  df <- data.frame(col = c())
  df1 <- data.frame(col1 = c(1:20),
                    col2 = c(rep("A", 10), rep("B", 10)))

  l_df <- list(nodes = df,
               edges = df1)

  table_id <- "nodes"

  output_too_few_columns <- validate_table_specs(x = l_df,
                                                 requirements = requirements)

  expect_equal(output_too_few_columns[[table_id]]$n_cols, not_validated(want = requirements$table[[table_id]]$specs$n_cols,
                                                                        is = 0))

  expect_equal(output_too_few_columns[[table_id]]$n_rows, not_validated(want = requirements$table[[table_id]]$specs$n_rows,
                                                                        is = 0))

  expect_equal(output_too_few_columns[[table_id]]$frType, not_validated(want = requirements$table[[table_id]]$specs$frType,
                                                                        is = "none"))


  expect_false(output_too_few_columns$all_requirements_met)



  # second example where requirements are met
  df <- data.frame(col1 = c(1:20),
                   col2 = c(rep("A", 10), rep("B", 10)),
                   col3 = c(rep("A", 10), rep("B", 10)))
  df1 <- data.frame(cola = c(rep("A", 10), rep("B", 10)),
                    colb = c(rep("A", 10), rep("B", 10)))

  l_df <- list(nodes = df,
               edges = df1)

  table_id <- "nodes"

  output_enough_columns <- validate_table_specs(x = l_df,
                                                requirements = requirements)

  expect_equal(output_enough_columns[["nodes"]]$n_cols, validated())

  expect_equal(output_enough_columns[["nodes"]]$n_rows, validated())

  expect_equal(output_enough_columns[["nodes"]]$frType, validated())

  expect_equal(output_enough_columns[["edges"]]$n_cols, validated())

  expect_equal(output_enough_columns[["edges"]]$n_rows, validated())

  expect_equal(output_enough_columns[["edges"]]$frType, validated())


  expect_true(output_enough_columns$all_requirements_met)


  expect_null(validate_table_specs(x, requirements, validated_table_meta = FALSE))

})
