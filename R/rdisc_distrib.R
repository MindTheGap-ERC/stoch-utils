rdisc_distrib = function(n, weights, vals){
  #' @title simulate disc. distribution
  #'
  #' @param n sample size
  #' @param weights probabilities to draw a value. the i-th entry in weights is the probability to draw the i-th entry of vals
  #' @param vals values to draw from
  #'
  #' @returns vector of length n with samples drawn from vals according to weights
  #'

  s = stats::runif(n, min = 0, max = 1)
  cum_weights = cumsum(weights)
  samp = sapply(seq_len(n), function(i) vals[which.max(s[i] < cum_weights)] )
  return(samp)
}
