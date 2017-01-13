# Testing spl.R

#library(limnotools)
source("../../R/spl.R")

context("What is this, actually?")

test_that("spl divides wide intervals.", {

  expect_equal(spl(c(1,5,10,20),2),c(1,5,7,10,20))
  expect_equal(spl(c(1,5,10),1),c(1,3,5,10))
  expect_equal(spl(c(1,5,10),2),c(1,5,7,10))

})


