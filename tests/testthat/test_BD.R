source("./R/BD.R")
source("./R/WP.R")

require(testthat)

test_that("First sim value is y0",{
          y0 = -1
          x = 1:3
          expect_equal(BD(x,y0 = y0)$y[1],y0)
          expect_equal(BD(0:3,y0 = y0)$y[1],y0)
          expect_equal(BD(x, sigma = 1, mu = 0, y0 = y0)$y[1],y0)
          expect_equal(BD(x, sigma = 0, mu = 0, y0 = y0)$y[1],y0)
          expect_equal(BD(x, sigma = 0, mu = -1, y0 = y0)$y[1],y0)
          expect_equal(BD(x, sigma = 1, mu = 0, y0 = y0)$y[1],y0)
          })

test_that("Matches Wiener process",{
          x = 1:10
          y0 = 0
          set.seed(1)
          wp = WP(x, y0 = y0)
          set.seed(1)
          bd = BD(x , sigma = 1, mu = 0, y0 = y0)
          expect_equal(wp,bd)
          y0 = 3
          set.seed(1)
          wp = WP(x , y0 = y0)
          set.seed(1)
          bd = BD(x , sigma = 1, mu = 0, y0 = y0)
          expect_equal(wp,bd)
})

#### Checks on sigma ####

test_that("Fails for negative sigma",{
  expect_error(WP(1:3, sigma = -3))
}
)

test_that("For sigma = 0, result is linear",{
  x = 1:100
  sigma = 0
  mu = -1
  y0 = 3
  expect_equal(BD(x, sigma, mu, y0)$y, mu * x + (y0 - mu * x[1]))
})

test_that("For sigma = 1, mu = 0 matches Wiener process",{
  x = 1:100
  y0 = 0
  set.seed(1)
  wp = WP(x, y0 = y0)
  set.seed(1)
  bd = BD(x , sigma = 1, mu = 0, y0 = y0)
  expect_equal(wp,bd)
  y0 = - 42
  set.seed(1)
  wp = WP(x, y0 = y0)
  set.seed(1)
  bd = BD(x , sigma = 1, mu = 0, y0 = y0)
  expect_equal(wp,bd)
})



### Input checks ####


test_that("Fails for non-numeric mu and y0",
          {
            expect_error(BD(1:3, mu = NULL))
            expect_error(BD(1:3, y0 = NULL))
            expect_error(BD(1:3, mu = NULL, y0 = NULL))
          })
