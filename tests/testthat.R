Sys.setenv("R_TESTS" = "")
library(testthat)
library(rstansim)

test_check("rstansim")
