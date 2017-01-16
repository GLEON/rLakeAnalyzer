# Testing zerge.R

source("../../R/zerge.R")

context("Testing the zerge() function.")

test_that("zerge merges in the middle.", {
  expect_equal( zerge(i=2,ni=c(1,5,10,20,28)), c(1,5,20,28) )
})

test_that("zerge merges the first interval.", {
  expect_equal( zerge(1,c(1,5,10,20)), c(1,10,20) )
})

test_that("zerge merges the second-to-last interval.", {
  expect_equal( zerge(2,c(1,5,10,20)), c(1,5,20) )
})

