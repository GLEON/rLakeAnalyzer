context("Testing wtr_layer")

test_that("Ensure that unconstrained and specified layer approach result in the same answer", {
  expect_equal( wtr_layer(depth=latesummer$depth, measure = latesummer$temper), 
                wtr_layer(depth=latesummer$depth, measure = latesummer$temper, 
                          nseg=wtr_layer(depth=latesummer$depth, measure = latesummer$temper)$nseg))
})


test_that("NA's are returned when profile has less than 30 readings", {
  # Data with less than 10 readings
  z = 1:25; sigmavar = rnorm(z)
  expect_warning(wtr_layer(depth=z, measure = sigmavar))
})

