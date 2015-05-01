
context("'cpt' and 'cpt.cols' objects")

good.pal <- 'OrRd'
bad.pal <- 'OrangeRed'
good.num <- 5
bad.num <- 999

# 'cpt' objects
c.checked <- cpt(good.pal, good.num, check.url = TRUE)
c.unchecked <- cpt(good.pal, good.num, check.url = FALSE)

# 'cpt.cols' objects
cc.checked <- read_cpt(c.checked)
cc.unchecked <- read_cpt(c.unchecked)


test_that("'cpt' objects are created appropriately",{
    
  # Class OK
  expect_is(c.checked, 'cpt')
  expect_is(c.unchecked, 'cpt')
  
  # status is checked as expected
  expect_true(is.na(c.unchecked[['cpt.url.status']]))
  expect_false(is.na(c.checked[['cpt.url.status']]))
  
  # We should expect errors if the palette checker is working correctly
  expect_error(cpt(bad.pal, good.num))
  expect_error(cpt(good.pal, bad.num))
  # last argument shouldnt matter
  expect_error(cpt(bad.pal, good.num, check.url = FALSE))
  expect_error(cpt(good.pal, bad.num, check.url = FALSE))
  
})


test_that("'cpt.cols' objects are created appropriately",{
  
  expect_is(cc.checked, 'cpt.cols')
  expect_is(cc.unchecked, 'cpt.cols')

})

test_that("'cpt.cols' object coercion is correct",{
  
  expect_is(as.data.frame(cc.checked), 'data.frame')
  expect_is(data.frame(cc.checked), 'data.frame')
  
  expect_identical(as.data.frame(cc.checked), data.frame(cc.checked))
  expect_equal(as.character(cc.checked), as.vector(cc.checked))
  
  expect_equal(length(as.character(cc.checked)), good.num)
  expect_equal(length(as.character(cc.unchecked)), good.num)
  expect_equal(length(as.character(cc.checked)), length(cc.checked))
  
})

