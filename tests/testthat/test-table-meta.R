test_that("data type validated", {

  # if more than one table is expected, data type needs to be a list of dataframes,
  # otherwise it can be a single dataframe or a list containing single dataframe
  df <- data.frame(col = c('a', 'b'))
  l_df <- list(df = df)
  l_df2 <- list(df1 = df,
                df2 = df)
  v <- c('a', 'b')
  l_v <- list(v)

  expect_equal(validate_data_type(df, n_tables_expected = 1), validated())
  expect_equal(validate_data_type(df, n_tables_expected = 2), not_validated(want = "list of dataframes",
                                                                            is = "data.frame"))

  expect_equal(validate_data_type(l_df, n_tables_expected = 1), validated())
  expect_equal(validate_data_type(l_df, n_tables_expected = 2), validated())

  expect_equal(validate_data_type(l_df2, n_tables_expected = 1), validated())
  expect_equal(validate_data_type(l_df2, n_tables_expected = 2), validated())

  expect_equal(validate_data_type(v, n_tables_expected = 1), not_validated(want = "data.frame",
                                                                           is = "character"))
  expect_equal(validate_data_type(v, n_tables_expected = 2), not_validated(want = "list of dataframes",
                                                                           is = "character"))

  expect_equal(validate_data_type(l_v, n_tables_expected = 1), not_validated(want = "data.frame",
                                                                             is = "list of character"))
  expect_equal(validate_data_type(l_v, n_tables_expected = 2), not_validated(want = "list of dataframes",
                                                                             is = "list contains non-dataframes"))


})

test_that("number of tables validated", {

  df <- data.frame(col = c('a', 'b'))
  l_df <- list(df = df)
  l_df2 <- list(df1 = df,
                df2 = df)

  expect_equal(validate_n_tables(df, n_tables_expected = 1), validated())
  expect_equal(validate_n_tables(df, n_tables_expected = 2), not_validated(want = 2,
                                                                           is = 1))

  expect_equal(validate_n_tables(l_df, n_tables_expected = 1), validated())
  expect_equal(validate_n_tables(l_df, n_tables_expected = 2), not_validated(want = 2,
                                                                             is = 1))

  expect_equal(validate_n_tables(l_df2, n_tables_expected = 1), not_validated(want = 1,
                                                                              is = 2))
  expect_equal(validate_n_tables(l_df2, n_tables_expected = 2), validated())

  expect_equal(validate_n_tables(df, n_tables_expected = 1, validated_data_type = FALSE), not_validated(want = 1,
                                                                                                        is = "none"))
})


test_that("table ids validated", {

  df <- data.frame(col = c('a', 'b'))
  l_df <- list(df = df)
  l_df2 <- list(df1 = df,
                df2 = df)
  l_df2a <- list(df,
                 df)

  expect_equal(validate_table_ids(df, n_tables_expected = 1), NULL)

  expect_equal(validate_table_ids(l_df2, n_tables_expected = 2, table_ids_expected = c("df1", "df2")),
               validated())

  expect_equal(validate_table_ids(l_df2, n_tables_expected = 2, table_ids_expected = c("df", "df2")),
               not_validated(want = c("df", "df2"),
                             is = c("df1", "df2")))

  expect_equal(validate_table_ids(l_df2a, n_tables_expected = 2, table_ids_expected = c("df1", "df2")),
               not_validated(want = c("df1", "df2"),
                             is = "unnamed tables"))

  expect_equal(validate_table_ids(df, n_tables_expected = 2, table_ids_expected = c("df1", "df2"), validated_data_type = FALSE),
               not_validated(want = c("df1", "df2"),
                             is = "none"))

  expect_equal(validate_table_ids(df, n_tables_expected = 2, table_ids_expected = c("df1", "df2"), validated_n_tables = FALSE),
               not_validated(want = c("df1", "df2"),
                             is = "none"))

})


test_that("table meta validated", {

  # load requirements
  path <- system.file("test_dsvalidate", "ex03-network", "dsvalidate", package = "dsvalidate")
  requirements <- requirements_load(path = path)

  df <- data.frame(col = c(LETTERS[1:19]))
  df2 <- data.frame(col1 = c(1:20),
                    col2 = c(rep("A", 10), rep("B", 10)))

  l_df <- list(nodes = df, edges = df2)
  l_df2 <- list(nodes = df, somethingelse = df2)


  output_insufficient_tables <- validate_table_meta(x = df,
                                                    requirements = requirements)

  output_wrong_ids <- validate_table_meta(x = l_df2,
                                          requirements = requirements)

  output_enough_tables <- validate_table_meta(x = l_df,
                                              requirements = requirements)


  expect_equal(output_insufficient_tables$n_tables, not_validated(want = 2,
                                                                  is = "none"))

  expect_equal(output_insufficient_tables$table_ids, not_validated(want = names(requirements$table),
                                                                   is = "none"))

  expect_equal(output_wrong_ids$table_ids, not_validated(want = names(requirements$table),
                                                         is = c("nodes", "somethingelse")))

  expect_equal(output_enough_tables$data_type, validated())
  expect_equal(output_enough_tables$n_tables, validated())
  expect_equal(output_enough_tables$table_ids, validated())

  expect_false(output_insufficient_tables$all_requirements_met)
  expect_false(output_wrong_ids$all_requirements_met)
  expect_true(output_enough_tables$all_requirements_met)

})

