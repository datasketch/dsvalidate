test_that("requirements loaded", {

  # load requirements
  folder <- system.file("test_dsvalidate", "ex03-network", package = "dsvalidate")
  file_path <- file.path(path,"requirements.yaml")
  requirements <- yaml::read_yaml(file_path)



})
