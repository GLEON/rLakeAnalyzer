# Testing spl.R

library(devtools) #load devtools
library(testthat)

#load_all will reload all R code in R/ that have changed since it was last run
load_all()



context("Testing the zerge() function.")

test_that("zerges (merges) the same interval created by spl()", {
  ni_tmp <- c(1,5,10,20)
  expect_equal(zerge(ni=spl(ni=ni_tmp,i=2), i=2), ni_tmp)
})

