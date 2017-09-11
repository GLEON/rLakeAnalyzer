context("Testing s_m_p.")

test_that("s_m_p gets the same answers as the Fortran.", {
  # Note that this is a pretty bogus answer, but that's what the
  # Fortran program produces...
  # print("Testing smp.")
  eps <- .01
  x <- c(0.0, 5.0, 10.0)
  y <- c(1.0, 10.0, 10.0)
  expect_equal(s_m_p(eps, x, y), c(1, 3, 3))
  # print("Still testing smp.")
  eps <- .01
  x <- c(0.0, 5.0, 10.0, 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 45.0)
  y <- c(1.0, 2.0, 3.0, 4.0, 5.0, 5.0, 4.0, 4.0, 3.0, 2.0)
  expect_equal(s_m_p(eps, x, y), c(1, 3, 6, 8, 10))
  # print("Yet still testing smp.")

  N <- 100
  eps <- .01
  angle <- (0:(N - 1)) * 2 * pi / N
  x <- angle
  y <- sin(angle)

  result <- c(1, 3, 12, 19, 26, 32, 38, 44, 59, 65, 71, 77, 82, 87, 93, 100)
  expect_equal(s_m_p(eps, x, y), result)
  # print("No longer testing smp.")
})

# test_that("s_m_p fails when x is not an increasing vector", {
#  eps = .005
#  x1 <- c(0.0,1.5,2.5,4.5,6.0,5.0,7.1,9.7,10.1,12.0) ## Non increasing vector
#  y = c(2.0,7.0,3.0,4.0,5.0,7.0,7.1,8.3,9.1,10.0)
#  expect_error(s_m_p(eps,x1,y), "x is not an increasing vector")
# })
