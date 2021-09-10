test_that("clean_numeric_iqr works", {
  
  x <- c(0:10,100)
  
  y <- clean_numeric_iqr(x)
  
  expect_equal(y, c(x[1:11] , NA))
  
  
})
