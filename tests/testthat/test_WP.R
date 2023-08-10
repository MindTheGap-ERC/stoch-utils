source("./R/WP.R")
require(testthat)

test_that("Length of y0 must be 1",{
  x = 1:3
  expect_error(WP(x, y0 = c()))
  expect_error(WP(x, y0 = c(1,2)))
})

test_that("y0 must be numeric & finite",{
  x = 1:3
  expect_error(WP(x, y0 = NULL))
  expect_error(WP(x, y0 = NA))
  expect_error(WP(x = 1:3, y0 = NaN))
  expect_error(WP(x = 1:3, y0 = Inf))
  expect_error(WP(x = 1:3, y0 = -Inf))
} )

test_that("x must have two or more entries",
          {
            expect_error(WP(1))
            expect_error(WP(c()))
          })

test_that("All elements of x must be numeric & finite",{
  expect_error(WP(c("A","B")))
  expect_error(WP(c(1,NA)))
  expect_error(WP(c(1,NaN)))
  expect_error(WP(c(1,Inf)))
  expect_error(WP(c(1,- Inf )))
})

test_that("x must be strictly increasing",
          {expect_error(WP(x = rev(1:3)))
           expect_error(WP(c(1,1,2)))})



test_that("First WP value is y0",
          {
            y0 = 0
            x = 1:10
            expect_equal(WP(x,y0)$y[1],y0)
            y0 = -3
            expect_equal(WP(x,y0 = y0)$y[1],y0)
          })

test_that("increments are normally distributed",
          {
  WP_incr = diff(WP(x = 0:1000)$y)
  cdf = function(x) pnorm(x)
  p = ks.test(WP_incr, cdf)$p.value
  expect_gt(p, 0.05)
})
