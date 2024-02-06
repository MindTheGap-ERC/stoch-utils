BD = function(x, sigma = 1, mu = 0, y0 = 0){
  #'
  #'@title simulate Brownian drift (BD)
  #'
  #'@param x numeric vector, strictly increasing entries
  #'@param sigma numeric >= 0
  #'@param mu numeric
  #'@param y0 numeric, value of the BD at
  #'
  #' @return a list with components x and y. y are the values of the BD
  #' at the ordinates x

  stopifnot(sigma >= 0)
  stopifnot(is.numeric(mu) , is.numeric(y0))
  WP_path = WP(x, y0 = 0)$y
  return(list(x = x,
              y = sigma * WP_path + mu * (x-min(x)) + y0))

}
