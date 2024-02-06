OU = function(from, to, mu, theta, sigma, y0 = "stationary", by = ((to - from)/(length.out - 1)), length.out = NULL){
  #'
  #'@title Simulate Ornstein-Uhlenbeck (OU) process
  #'
  #'@param from numeric
  #'@param to numeric
  #'@param mu numeric, long term mean
  #'@param theta numeric, mean reversion speed
  #'@param sigma numeric >= 0, strength of randomness
  #'@param y0 either string "stationary"or a finite numberic value. If "stationary"
  #' the OU will start in the stationary distribution, else y0 is the initial value (value of the OU at from)
  #'@param by numeric
  #'@param length.out int
  #'
  #'@returns a list with components x and y. x contains where the OU is evaluated, y the values of the OU process
  #'
  #'@details simulation via EUler-Maruyama method, see
  #' https://en.wikipedia.org/w/index.php?title=Euler%E2%80%93Maruyama_method&oldid=1135099937


  x = seq(from, to, by)

  if (y0 == "stationary"){
    y0 = stats::rnorm(1, mean = mu, sd = sigma / sqrt(2 * theta))
  }

  noiseIncrements <- stats::rnorm(
    n = length(x) - 1,
    mean = 0,
    sd = sqrt(by)
  )
  ouval <- rep(NA, length(x))
  ouval[1] <- y0
  for (j in 2:length(x)) {
    ouval[j] <- ouval[j - 1] + by * (theta * (mu - ouval[j - 1])) +
      sigma * noiseIncrements[j - 1]
  }
  return(list(x = x,
              y = ouval))

}
