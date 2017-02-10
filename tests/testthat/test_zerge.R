context("Testing the merge() function.")

test_that("merge merges in the middle.", {
  expect_equal( merge(i=3,ni=c(1,5,10,20,28)), c(1,5,20,28) )
})

test_that("merge merges the first interval.", {
  expect_equal( merge(2,c(1,5,10,20)), c(1,10,20) )
})

test_that("merge merges the second-to-last interval.", {
  expect_equal( merge(3,c(1,5,10,20)), c(1,5,20) )
})

