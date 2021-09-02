
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dsvalidate

<!-- badges: start -->

<!-- badges: end -->

The goal of dsvalidate is to validate data to ensure correct data
format, column names, data types and column properties for Datasketch
apps.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("datasketch/dsvalidate")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(dsvalidate)
#> Loading required package: validate

path <- system.file("test_dsvalidate", "ex02-network", "dsvalidate", package = "dsvalidate")
  requirements <- requirements_load(path = path)

  df <- data.frame(id = c(1:20),
                   b = c(rep("A", 10), rep("B", 10)),
                   c = c(rep("A", 10), rep("B", 10)))
  df1 <- data.frame(col1 = c(rep("A", 5), rep("B", 5), rep("C", 10)),
                    col2 = c(rep("A", 10), rep("B", 10)))
  x <- list(nodes = df,
            edges = df1)
# 
#   validate <- validate_requirements(x = x,
#                                     requirements = requirements)
# 
#   # validate table meta data
#   validate$table
# 
#   # validate table specifications for each table
#   validate$specs
# 
#   # validate if field requirements are met
#   validate$fields
# 
#   # validate if ALL requirements are met
#   validate$all_requirements_met
```
