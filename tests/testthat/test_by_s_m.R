context("Testing by_s_m.")

test_that("Run, compare results with Fortran.", {
  load("../../data/t11.rda")   # Same data as Fortran test
  # by_s_m = function(thres,z0,zmax,z,sigma)
  results = by_s_m( thres=.005, z0=2.5,zmax=140, z=t11$depth, sigma=t11$density)
  # results = list(nimax=nimax,smz=smz,sms=sms,by_s_m=ss)
  # print(resdf)


  source("by_s_m_results.R") # From Fortran
  # print(DensityResults)
  expect_equal( results$nimax, DensityResults$nimax)
  expect_equal( results$by_s_m, DensityResults$result)

  expect_lt( sqrt(sum(results$smz-DensityResults$smz)), .00001)
  expect_lt( sqrt(sum(results$sms-DensityResults$sms)), .00001)
})
