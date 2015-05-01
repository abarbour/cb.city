
context("Convenience functions")

test_that("city_palette results are OK",{
  n <- 5
  p <- 'OrRd'
  cc. <- read_cpt(cpt(p, n))
  cc.convenient <- city_palette(p, n)
  expect_is(cc.convenient, 'cpt.cols')
  expect_identical(cc., cc.convenient)
})