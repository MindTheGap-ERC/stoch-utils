WP = function(x, y0 = 0){
  #'
  #'@title simulate Wiener process
  #'
  #'@param x: numeric vector, strictly increasing entries
  #'@param: y0: numeric, value of the process at x[0]
  #'
  #'@details
    #'to simulate a Wiener process in the strict mathematical sense, choose x 
    #' with x[1] = 0 and y0 = 0 (default)
    #'
  #'
  #' @returns A list with components x and y. x is a duplicate of the input x, 
  #' the abscissa where the Wiener process is evaluated. y are the values (ordinate)
  #' the WP at x
  #' 
  stopifnot(length(y0) == 1)
  stopifnot(is.numeric(y0) , is.finite(y0) , !is.null(y0))
  stopifnot(all(is.finite(x)), is.numeric(x))
  stopifnot(length(x) >= 2)
  increments = diff(x)
  stopifnot(all(increments > 0) )
  
  return(list(x = x,
              y = cumsum(c(y0, rnorm(n = length(x) - 1, mean = 0, sd = sqrt(increments))))))
  
}