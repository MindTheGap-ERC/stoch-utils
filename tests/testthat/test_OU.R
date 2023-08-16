require(testthat)

source("./R/OU.R")
test_seed = 1
test_that("Value at from is y0 for deterministic values",{
  from = 0
  to = 1000
  length.out = 10
  sigma = 1
  theta = 1
  mu = 0
  y0 = 1
  expect_equal(object = mean(OU(from, to, mu, theta, sigma, y0 = y0,length.out = length.out)$y[1]),
               expected = y0)
  y0 = 0
  expect_equal(object = mean(OU(from, to, mu, theta, sigma, y0 = y0,length.out = length.out)$y[1]),
               expected = y0)
})

test_that("length.out and by return same results",{
  sigma = 1
  theta = 1
  mu = 0
  from = 0
  to = 1
  set.seed(test_seed)
  ou1 = OU(from, to, mu, theta, sigma, length.out = 11)
  set.seed(test_seed)
  ou2 = OU(from, to, mu, theta, sigma, by = 0.1)
  expect_equal(ou1, ou2)
})



test_that("For stationary initial conditions, initial value is correctly randomized",{
  n_rep = 1000
  sigma = 1
  theta = 1
  mu = 0
  from = 0
  to = 1
  length.out = 2
  ini_vals = sapply(1:n_rep, function(x) OU(from, to, mu, theta, sigma, length.out = length.out)$y[1])
  cdf = function(x) pnorm(x, mean = mu, sd = sigma / sqrt(2 * theta))
   p = ks.test(ini_vals, cdf)$p.value
   expect_gte(p, 0.05)
})

test_that("Distribution under stationary start is correct",{
  n_rep = 1000
  sigma = 2
  theta = 1
  mu = 1
  from = 0
  to = 1
  length.out = 100
  vals = sapply(1:n_rep, function(x) OU(from, to, mu, theta, sigma, length.out = length.out)$y[length.out])
  cdf = function(x) pnorm(x, mean = mu, sd = sigma / sqrt(2 * theta))
  p = ks.test(vals, cdf)$p.value
  expect_gte(p, 0.05)
})




test_that("for stationary version, mean is mu",{
  from = 0
  to = 100000
  length.out = 100000
  sigma = 1
  theta = 1
  mu = 0
  expect_equal(object = mean(OU(from, to, mu, theta, sigma, length.out = length.out)$y),
               expected = mu,
               tolerance = 0.01)
  
  mu = 10
  expect_equal(object = mean(OU(from, to, mu, theta, sigma, length.out = length.out)$y),
               expected = mu,
               tolerance = 0.01)
})

