# Testing spl.R

library(limnotools)

context("What is this, actually?")

test_that("spl divides wide intervals.", {

  expect_equal(spl(2,c(1,5,10,20)),c(1,5,7,10,20))
  expect_equal(spl(1,c(1,5,10)),c(1,2,5,10))
  expect_equal(spl(2,c(1,5,10)),c(1,5,7,10))

})


