context("Testing the getxxnorm() function.")

test_that("getxxnorm agrees with Fortran.", {
  x = c(1.0, 2.0, 3.0, 4.5)
  y = c(10,  20,  10,  5)
  nn = 6
  x0=0
  dx=1
  # print(x)
  # print(y)
  df = getxxnorm(x,y,nn,x0,dx)
  expect_equal( df$anormx[1], 5 )
  expect_equal( df$anormy[1], -6.6666666 )
  expect_equal( df$x, .2*(0:(nn-1)) )
  expect_equal( df$y, c(0.00000000,0.00000000,-1.5000000,0.00000000,0.500000000,1.00000000) )
  # print(df)
})

test_that("merge_intervals merges the first interval.", {
  expect_equal( merge_intervals(2,c(1,5,10,20)), c(1,10,20) )
})

test_that("merge_intervals merges the second-to-last interval.", {
  expect_equal( merge_intervals(3,c(1,5,10,20)), c(1,5,20) )
})
