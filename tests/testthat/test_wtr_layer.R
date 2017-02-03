context("Testing wtr_layer")

test_that("Ensure that unconstrained and specified segment approach result in the same answer", {
  expect_equal( wtr_layer(depth=latesummer$depth, measure = latesummer$temper), 
                wtr_layer(depth=latesummer$depth, measure = latesummer$temper, 
                          nseg=wtr_layer(depth=latesummer$depth, measure = latesummer$temper)$nseg))
})
