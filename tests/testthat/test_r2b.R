# Testing r2b.R

library(devtools) #load devtools
library(testthat)

#load_all will reload all R code in R/ that have changed since it was last run
load_all()
#source("../../R/r2b.R")

context("Testing the r2b() function.")

x = c(0.0,1.5,2.5,4.5,5.0,6.0,7.1,9.7,10.1,12.0)
y = c(2.0,7.0,3.0,4.0,5.0,7.0,7.1,8.3,9.1,10.0)

test_that("r2b gets the right answers. Duh.", {
  expect_equal( round(r2b(1,length(x),x,y),digits=6), 3.833333)
})

