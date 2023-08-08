source("src/CRPPP.R")

require(testthat)

test_that("Distance between points is exponentially distributed",{
  pval_KS_compared_to_exp = function(from, to, rate){
    diff = diff(CRPPP(from, to,  rate))
    cdf = function(x) pexp(x, rate )
    return(ks.test(diff, cdf)$p.value)
  }
          expect_gte(pval_KS_compared_to_exp(from = 0, to = 100, rate = 10), 0.05)
  })

# for rate 0, empty num vector
test_that("Edge cases return vector of length 0",
          {
expect_length(CRPPP(from = 0, to = 1, rate = 0), 0)
expect_length(CRPPP(from = 0, to = 0, rate = 0), 0)
})

# numeric return argument
test_that("Returns numeric vector",
          {
expect_vector(CRPPP(from = 0, to = 1, rate = 0), ptype = numeric())
            expect_vector(CRPPP(from = 0, to = 0, rate = 1), ptype = numeric())
            expect_vector(CRPPP(from = 0, to = 0, rate = 0), ptype = numeric())
            expect_vector(CRPPP(from = 0, to = 100, rate = 1), ptype = numeric())
          })

# error for neg rate
test_that("requires increasing range an non-negative rates",
          {
expect_error(CRPPP(from = 0, to = 1, rate = -1))
expect_error(CRPPP(from = 0, to = -1, rate = 1))
})



