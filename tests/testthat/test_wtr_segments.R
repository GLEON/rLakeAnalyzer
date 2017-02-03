context("Testing wtr_segments")

test_that("Ensure that unconstrained and specified segment approach result in the same answer", {
  expect_equal( wtr_segments(depth=latesummer$depth, measure = latesummer$temper), 
                wtr_segments(depth=latesummer$depth, measure = latesummer$temper, 
                          nseg=unique(wtr_segments(depth=latesummer$depth, measure = latesummer$temper)$nseg)))
})
