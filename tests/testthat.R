Sys.setenv("R_TESTS" = "")
library(testthat)
library(campsisqual)
test_check("campsisqual")
