context("Testing wtr.layer")
data("latesummer")

test_that("Ensure that unconstrained and specified layer approach result in the same answer", {
  expect_equal(
    wtr.layer(depth = latesummer$depth, measure = latesummer$temper)[, 1:4],
    wtr.layer(
      depth = latesummer$depth, measure = latesummer$temper,
      nseg = wtr.layer(depth = latesummer$depth, measure = latesummer$temper)$nseg
    )[, 1:4]
  )
})


test_that("NA's are returned when profile has less than 30 readings", {
  # Data with less than 10 readings
  z <- 1:25
  sigmavar <- rnorm(z)
  expect_warning(wtr.layer(depth = z, measure = sigmavar))
  expect_warning(wtr.layer(data = latesummer[1:20, ], depth = depth, measure = temper))
})

test_that("Omitting the data argument results in a valid data.frame with constrained and uncontrained scenarios", {
  expect_silent(wtr.layer(depth = latesummer$depth, measure = latesummer$temper))
  expect_silent(wtr.layer(depth = latesummer$depth, measure = latesummer$temper, nseg = 5))
})

test_that("Including the data argument results in a valid data.frame with constrained and uncontrained scenarios", {
  expect_silent(wtr.layer(data = latesummer, depth = depth, measure = temper))
  expect_silent(wtr.layer(data = latesummer, depth = depth, measure = temper, nseg = 5))
})
