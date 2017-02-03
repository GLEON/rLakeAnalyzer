context("Testing by_s_m.")

source("by_s_m_results.R") # Get results from Fortran test.
load("../../data/t11.rda")   # Same data as Fortran test

error = function(veca, vecb) {
  sqrt(sum( (veca-vecb)^2 ))
}

maxerror = 1.e-6

test_that("Run, compare density results with Fortran.", {
  # by_s_m = function(thres,z0,zmax,z,sigma)
  results = by_s_m( thres=.005, z0=2.5,zmax=140, z=t11$depth, sigma=t11$density)
  # results = list(nimax=nimax,smz=smz,sms=sms,by_s_m=ss)

  expect_equal( results$nimax, DensityResults$nimax )
  expect_equal( results$by_s_m, DensityResults$result )

  expect_lt( error(results$smz,DensityResults$smz), maxerror )
  expect_lt( error(results$sms,DensityResults$sms), maxerror )
})

test_that("Run, compare temperature results with Fortran.", {
  # by_s_m = function(thres,z0,zmax,z,sigma)
  results = by_s_m( thres=.005, z0=2.5,zmax=140, z=t11$depth, sigma=t11$temper)
  # results = list(nimax=nimax,smz=smz,sms=sms,by_s_m=ss)

  expect_equal( results$nimax, TemperatureResults$nimax )
  expect_equal( results$by_s_m, TemperatureResults$result )

  expect_lt( error(results$smz,TemperatureResults$smz), maxerror )
  expect_lt( error(results$sms,TemperatureResults$sms), maxerror )
})

test_that("Run, compare salinity results with Fortran.", {
  # by_s_m = function(thres,z0,zmax,z,sigma)
  results = by_s_m( thres=.005, z0=2.5,zmax=140, z=t11$depth, sigma=t11$salinity)
  # results = list(nimax=nimax,smz=smz,sms=sms,by_s_m=ss)

  expect_equal( results$nimax, SalinityResults$nimax )
  expect_equal( results$by_s_m, SalinityResults$result )

  expect_lt( error(results$smz,SalinityResults$smz), maxerror )
  expect_lt( error(results$sms,SalinityResults$sms), maxerror )
})
