context("Testing the split_interval() function.")

test_that("split_interval divides wide intervals.", {
  expect_equal(split_interval(c(1,5,10,20),2),c(1,5,7,10,20))
  expect_equal(split_interval(c(1,2,6,10),2),c(1,2,4,6,10))
})

test_that("split_interval divides the first and last intervals.", {
  expect_equal(split_interval(c(1,5,10),1),c(1,3,5,10))
  expect_equal(split_interval(c(1,5,10),2),c(1,5,7,10))
})

test_that("split_interval divides even,even intervals.", {
  expect_equal(split_interval(c(1,2,6,10),2),c(1,2,4,6,10))
})

test_that("split_interval divides even,odd intervals.", {
  expect_equal(split_interval(c(1,2,5,10),2),c(1,2,4,5,10))
})

test_that("split_interval divides odd,even intervals.", {
  expect_equal(split_interval(c(1,3,6,10),2),c(1,3,5,6,10))
})

test_that("split_interval divides odd,odd intervals.", {
  expect_equal(split_interval(c(1,3,7,10),2),c(1,3,5,7,10))
})

test_that("interval number is less than length(ni)", {
  expect_error(split_interval(c(1,3,6,10),5))
})




