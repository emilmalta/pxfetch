# This file is the entry point for R CMD check's test runner.
# You generally don't need to edit it.
library(testthat)
library(waldo)
library(pxfetch)

test_check("pxfetch")
