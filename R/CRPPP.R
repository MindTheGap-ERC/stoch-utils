CRPPP = function(from, to, rate){
  #'
  #' @title Simulate constant rate (CR) poisson point process (PPP)
  #'
  #' @param from numeric
  #' @param to numberi
  #' @param rate rate of the PPP
  #'
  #' @returns numeric vector of length >= 0. points of the PPP
  stopifnot(rate >= 0)
  stopifnot(from <= to)
  no_of_points = stats::rpois(n = 1, lambda =  rate * (to - from))
  points = stats::runif(n = no_of_points, min = from, max = to)
  return(sort(points))
}
