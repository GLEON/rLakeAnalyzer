context("Testing wtr_layer")

test_that("Ensure that unconstrained and specified segment approach result in the same answer", {
  expect_equal( wtr_layer(depth=latesummer$depth, measure = latesummer$temper), 
                wtr_layer(depth=latesummer$depth, measure = latesummer$temper, 
                          nseg=wtr_layer(depth=latesummer$depth, measure = latesummer$temper)$nseg))
})


test_that("Ensures that increasing depth vectors are extracted from non-increasing vectors; order_seq is an internal function in wtr_layers",{
  z <- c(1,2,1,0.5,1,4,2,1:10,2,3)
  
  ## Internal function in wtr_layer
  order_seq <- function(x) {
    s = 1L + c(0L, which( x[-1L] < x[-length(x)] ), length(x))
    w = which.max(diff(s))
    return(s[w]:(s[w+1]-1L))
    }
  
  expect_equal(z[order_seq(z)], 1:10)
})

test_that("NA's are returned when profile has less than 10 readings", {
  # Data with less than 10 readings
  z = 1:8; sigmavar = rnorm(z)
  expect_warning(wtr_layer(depth=z, measure = sigmavar))
})

test_that("Unsorted vector fails", {
  z <- c(1,2,1,0.5,1,4,2,1:10,2,3); sigmavar = rnorm(z)
  expect_warning(wtr_layer(depth = z, measure =sigmavar))
})
  

