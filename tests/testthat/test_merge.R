context("Testing the merge_intervals() function.")

test_that("merge_intervals merges in the middle.", {
  expect_equal(merge_intervals(i = 3, ni = c(1, 5, 10, 20, 28)), c(1, 5, 20, 28))
})

test_that("merge_intervals merges the first interval.", {
  expect_equal(merge_intervals(2, c(1, 5, 10, 20)), c(1, 10, 20))
})

test_that("merge_intervals merges the second-to-last interval.", {
  expect_equal(merge_intervals(3, c(1, 5, 10, 20)), c(1, 5, 20))
})
