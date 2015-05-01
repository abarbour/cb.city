
context("Status of the assumed cpt-city urls")

ops <- getOption('cb.city.ops')

test_that("status of the cpt-city urls", {
  
  bu <- ops[['base.url']]
  expect_true(httr::url_ok(bu))
  
  cbu <- ops[['cb.url']]
  expect_true(httr::url_ok(cbu))
  
})

test_that("status of the cpt-city palette classification urls", {
  
  # palette classifications
  u <- ops[['cb.url']]
  uexts <- ops[['cb.url.exts']]
  for (ue in uexts){
    extu <- paste(u, ue, "index.html", sep="/")
    expect_true(httr::url_ok(extu))
  }
  
})
