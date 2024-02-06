

OUstat = function(from, to, mu, theta, sigma, by = ((to - from)/(length.out - 1)), length.out = NULL){
  #'
  #'@title Simulate stationary Ornstein-Uhlenbeck (OU) process
  #'
  #'@param from numeric
  #'@param to numeric
  #'@param mu numeric
  #'@param theta numeric
  #'@param sigma numeric
  #'@param by numeric
  #'@param length.out int
  #'
  #'@returns a list with components x and y. x contains where the OU is evaluated, y the values of the OU process
  #'
  #'@details simulation via EUler-Maruyama method, see
  #' https://en.wikipedia.org/w/index.php?title=Euler%E2%80%93Maruyama_method&oldid=1135099937
  #' initial value x0 is chosen to make the process stationary
  x = seq(from, to, by)
  noiseIncrements <- stats::rnorm(
    n = length(x) - 1,
    mean = 0,
    sd = sqrt(by)
  )
  x0 = stats::rnorm(1,
             mean = mu,
             sd = sigma / sqrt( 2 * theta))
  ouval <- rep(NA, length(x))
  ouval[1] <- x0
  for (j in 2:length(x)) {
    ouval[j] <- ouval[j - 1] + by * (theta * (mu - ouval[j - 1])) +
      sigma * noiseIncrements[j - 1]
  }
  return(list(x = x,
              y = ouval))
}


