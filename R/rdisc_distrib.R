rdisc_distrib = function(n, weights, vals){
  #' @title simulate disc. distribution
  #' 
  #' @param n sample size
  #' @param weights probabilities to draw a value. weights[i] is the probability to draw vals[i]
  #' @param vals values to draw from
  #' 
  #' @returns vector of length n with samples drawn from vals according to weights
  #' 
  
  s = runif(n, min = 0, max = 1)
  cum_weights = cumsum(weights)
  samp = sapply(seq_len(n), function(i) vals[which.max(s[i] < cum_weights)] )
  return(samp)
}