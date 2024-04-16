## Run all code for the project
renv::restore()

## Prepare raw data
source("./code/dataprep.r")

## Clean further and Describe data (Descriptive Analysis)
source("./code/datadesc.r")

## Run Regression Analysis
source("./code/analysis.r")


## Run Tests
testthat::local_edition(3)
testthat::test_dir("tests")