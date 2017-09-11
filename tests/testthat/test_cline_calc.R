context("Testing for warnings in cline_calc")

test_that("cline_calc throws a warning with this data", {
  depth <- c(1.56, 1.61, 20.5, 29.8)
  measure <- c(13.0, 12.8, 7.8, 7.4)
  expect_warning(cline_calc(depth, measure))
})
