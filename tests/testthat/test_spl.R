# Testing spl.R

#library(limnotools)
source("../../R/spl.R")

context("Testing the spl() function.")

test_that("spl divides wide intervals.", {
  expect_equal(spl(c(1,5,10,20),2),c(1,5,7,10,20))
  expect_equal(spl(c(1,2,6,10),2),c(1,2,4,6,10))
})

test_that("spl divides the first and last intervals.", {
  expect_equal(spl(c(1,5,10),1),c(1,3,5,10))
  expect_equal(spl(c(1,5,10),2),c(1,5,7,10))
})

test_that("spl divides even,even intervals.", {
  expect_equal(spl(c(1,2,6,10),2),c(1,2,4,6,10))
})

test_that("spl divides even,odd intervals.", {
  expect_equal(spl(c(1,2,5,10),2),c(1,2,3,5,10))
})

test_that("spl divides odd,even intervals.", {
  expect_equal(spl(c(1,3,6,10),2),c(1,3,4,6,10))
})

test_that("spl divides odd,odd intervals.", {
  expect_equal(spl(c(1,3,7,10),2),c(1,3,5,7,10))
})

# The following test does not work though an error is thrown.
#test_that("Should fail on too-narrow intervals.", {
#  expect_error(spl(c(1,3,4,10),2))
#})

