context("Testing by_s_m.")

test_that("Run, compare results with Fortran.", {
  load("../../data/t11.rda")   # Same data as Fortran test
  # by_s_m = function(thres,z0,zmax,z,sigma)
  resdf = by_s_m( thres=.005, z0=2.5,zmax=140, z=t11$depth, sigma=t11$density)
  print(resdf)

  source("by_s_m_results.R") # From Fortran
  print(DensityResults)
  expect_equal( t11,t11)

})
