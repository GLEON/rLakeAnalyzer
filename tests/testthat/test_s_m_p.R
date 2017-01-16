
# Testing s_m_p.R

x = c(0.0,1.5,2.5,4.5,5.0,6.0,7.1,9.7,10.1,12.0)
y = c(2.0,7.0,3.0,4.0,5.0,7.0,7.1,8.3,9.1,10.0)


test_that("s_m_p gets the same answers as the Fortran.", {
  # Note that this is a pretty bogus answer, but that's what the
  # Fortran program produces...
  eps1 = .01
  x1=c(0.0, 5.0, 10.0)
  y1=c(1.0, 10.0, 10.0)
  expect_equal( s_m_p(eps1,x1,y1), c(1,3,3) )

  eps = .01
  x = c(0.0, 5.0, 10.0, 15.0, 20.0, 25.0,30.0,35.0,40.0,45.0)
  y = c(1.0, 2.0, 3.0,   4.0,  5.0,  5.0, 4.0, 4.0, 3.0, 2.0)
  expect_equal( s_m_p(eps,x,y), c(1,3,6,8,10) )
})

