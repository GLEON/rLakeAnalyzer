context("Testing the getxxnorm() function.")

test_that("merge_intervals merges the first interval.", {
  expect_equal(merge_intervals(2, c(1, 5, 10, 20)), c(1, 10, 20))
})

test_that("merge_intervals merges the second-to-last interval.", {
  expect_equal(merge_intervals(3, c(1, 5, 10, 20)), c(1, 5, 20))
})
