test_that("data validated", {

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

  output_validate <- validate_requirements(x = x,
                                           requirements = requirements)

  # check if all meta table requirements met
  expect_true(output_validate$table$all_requirements_met)

  # check if all table specs requirements met
  expect_true(output_validate$specs$all_requirements_met)

  # check if all fields requirements are met in table 'nodes'
  expect_false(output_validate$fields$nodes$all_requirements_met)

  # check if all fields requirements are met in table 'edges'
  expect_true(output_validate$fields$edges$all_requirements_met)

  # check if all fields requirements
  expect_false(output_validate$fields$all_requirements_met)

  # CHECK IF ALL REQUIREMENTS ARE MET COMPLETELY
  expect_false(output_validate$all_requirements_met)

})

