context("Testing wtr_segments")

test_that("Ensure that unconstrained and specified segment approach result in the same answer", {
  expect_equal( wtr_segments(depth=latesummer$depth, measure = latesummer$temper), 
                wtr_segments(depth=latesummer$depth, measure = latesummer$temper, 
                          nseg=unique(wtr_segments(depth=latesummer$depth, measure = latesummer$temper)$nseg)))
})


test_that("NA's are returned when profile has less than 10 readings", {
  # Data with less than 10 readings
  z = 1:28; sigmavar = rnorm(z)
  expect_warning(wtr_segments(depth=z, measure = sigmavar))
})
