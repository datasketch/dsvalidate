test_that("requirements loaded", {

  path <- system.file("test_dsvalidate", "ex01-invalid_load", "dsvalidate", package = "dsvalidate")

  expect_error(requirements_load(path = path, filename = "requirements_empty.yaml"),
               "Requirements file empty.")

  expect_error(requirements_load(path = path, filename = "requirements_missing_table.yaml"),
               "Requirements file doesn't contain table specs.")

  expect_error(requirements_load(path = path, filename = "requirements_missing_table_id.yaml"),
               "All table requirements must include table_id, specs, and fields.")

  expect_error(requirements_load(path = path, filename = "requirements_missing_specs.yaml"),
               "All table requirements must include table_id, specs, and fields.")

  expect_error(requirements_load(path = path, filename = "requirements_missing_fields.yaml"),
               "All table requirements must include table_id, specs, and fields.")

  expect_silent(requirements_load(path = path))

  # test that n_rows is set to greater_than 0 when not specified
  requirements <- requirements_load(path = path)
  expect_equal(requirements$table$edges$specs$n_rows, list(greater_than = 0))

})
