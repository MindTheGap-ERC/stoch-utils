OU = function(from, to, mu, theta, sigma, x0, by = ((to - from)/(length.out - 1)), length.out = NULL){
  #'
  #'@title Simulate Ornstein-Uhlenbeck (OU) process
  #'
  #'@param from: numeric
  #'@param to: numeric
  #'@param mu: numeric
  #'@param theta: numeric
  #'@param x0: numeric
  #'@param by: numeric
  #'@param length.out: int
  #'
  #'@returns a list with components x and y. x contains where the OU is evaluated, y the values of the OU process
  #'
  #'@details simulation via EUler-Maruyama method, see 
  #' https://en.wikipedia.org/w/index.php?title=Euler%E2%80%93Maruyama_method&oldid=1135099937
  
  
  x = seq(from, to, by)
  noiseIncrements <- rnorm(
    n = length(x) - 1,
    mean = 0,
    sd = sqrt(by)
  )
  ouval <- rep(NA, length(x))
  ouval[1] <- x0
  for (j in 2:length(x)) {
    ouval[j] <- ouval[j - 1] + by * (theta * (mu - ouval[j - 1])) +
      sigma * noiseIncrements[j - 1]
  }
  return(list(x = x,
              y = ouval))
  
}

OUstat = function(from, to, mu, theta, sigma, by = ((to - from)/(length.out - 1)), length.out = NULL){
  #'
  #'@title Simulate stationary Ornstein-Uhlenbeck (OU) process
  #'
  #'@param from: numeric
  #'@param to: numeric
  #'@param mu: numeric
  #'@param theta: numeric
  #'@param x0: numeric
  #'@param by: numeric
  #'@param length.out: int
  #'
  #'@returns a list with components x and y. x contains where the OU is evaluated, y the values of the OU process
  #'
  #'@details simulation via EUler-Maruyama method, see 
  #' https://en.wikipedia.org/w/index.php?title=Euler%E2%80%93Maruyama_method&oldid=1135099937
  #' initial value x0 is chosen to make the process stationary
  x0 = rnorm(1,
             mean = mu,
             sd = sigma / sqrt( 2 * theta))
  return(OU(from, to, mu, theta, sigma, x0, by = ((to - from)/(length.out - 1)), length.out = NULL))
}

BM = function(x, y0 = 0){
  #'
  #'@title simulate Brownian motion
  #'
  #'@param x: numeric vector, strictly increasing entries
  #'@param: y0 numeric
  #'
  
  return(list(x = x,
              y = cumsum(c(y0, rnorm(n = length(x) - 1, mean = 0, sd = sqrt(diff(x)))))))
  
}

BD = function(x, sigma = 1, mu = 0, y0 = 0){
  #'
  #'@title simulate Brownian drift
  #'
  #'@param x: numeric vector, strictly increasing entries
  #'@param: sigma: numeric
  #'@param: mu: numeric
  #'@param: y0 numeric
  #'
  BM_path = BM(x, y0 = 0)$y
  return(list(x = x,
              y = sigma * BM_path + my * x + y0))
  
}

CRPPP = function(from, to, rate){
  #' 
  #' @title Simulate constant rate (CR) poisson point process (PPP)
  #' 
  #' @param from: numeric
  #' @param to: numberi
  #' @param rate: rate of the PPP
  #' 
  #' @returns numeric vector of length >= 0. points of the PPP
  
  no_of_points = rpois(n = 1, lambda =  rate * (to - from))
  return(runif(n = no_of_points, min = from, max = to))
}
