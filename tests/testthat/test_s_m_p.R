
# Testing s_m_p.R
library(limnotools)

x = c(0.0,1.5,2.5,4.5,5.0,6.0,7.1,9.7,10.1,12.0)
y = c(2.0,7.0,3.0,4.0,5.0,7.0,7.1,8.3,9.1,10.0)


test_that("s_m_p gets the same answers as the Fortran.", {
  # Note that this is a pretty bogus answer, but that's what the
  # Fortran program produces...
  eps = .01
  x=c(0.0, 5.0, 10.0)
  y=c(1.0, 10.0, 10.0)
  expect_equal( s_m_p(eps,x,y), c(1,3,3) )

  eps = .01
  x = c(0.0, 5.0, 10.0, 15.0, 20.0, 25.0,30.0,35.0,40.0,45.0)
  y = c(1.0, 2.0, 3.0,   4.0,  5.0,  5.0, 4.0, 4.0, 3.0, 2.0)
  expect_equal( s_m_p(eps,x,y), c(1,3,6,8,10) )

  N = 100
  eps = .01
  angle = (0:(N-1))*2*pi/N
  x = angle
  y = sin(angle)

  result = c(1,  3, 12, 19, 26, 32, 38, 44, 59, 65, 71, 77, 82, 87, 93,100)
  expect_equal( s_m_p(eps,x,y), result)
})

