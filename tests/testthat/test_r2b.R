context("Testing the r2b() function.")


test_that("r2b gets the right answers. Duh.", {
  x = c(0.0,1.5,2.5,4.5,5.0,6.0,7.1,9.7,10.1,12.0)
  y = c(2.0,7.0,3.0,4.0,5.0,7.0,7.1,8.3,9.1,10.0)
  digs = 16 # number of digits to the right of decimal place we expect to be the same.
  expect_equal( round(r2b(1,length(x),x,y),digits=digs), round(3.833333333333333,digs))

})

