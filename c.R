# o functie este densitate de probabilitate daca:
# 1. f(x,y) >=0
# 2. integrala pe (-Inf,+Inf) = 1

f_pozitiva <- function(f, interval_x = c(-Inf,Inf), interval_y = c(-Inf,Inf))
{
  # functia este mai mare decat 0
  # generam valori pentru x si y in intervalele date si calculam functia 
  # in valorile respective
  
  if(interval_x[1] == -Inf)
    interval_x[1] = 10^6
  if(interval_x[2] == Inf)
    interval_x[2] = 10^6
  if(interval_y[1] == -Inf)
    interval_y[1] = 10^6
  if(interval_y[2] == Inf)
    interval_y[2] = 10^6
  
  x_vals <- seq(interval_x[1], interval_x[2], len = 10^6)
  y_vals <- seq(interval_y[1], interval_y[2], len = 10^6)
  
  all(f(x_vals,y_vals)>=0)
}

este_densitate <- function(f, interval_x = c(-Inf,Inf), interval_y = c(-Inf,Inf))
{
  int <-  integrate(function(y) {
    sapply(y, function(y_val) {
      integrate(function(x) f(x, y_val), interval_x[1], interval_x[2])$value
    })
  }, interval_y[1], interval_y[2])
  er <- int$abs.error
  int <- int$value
  if(abs(int - 1) < er)
    return(f_pozitiva(f,interval_x,interval_y))
  return(FALSE)
}

interval_x <- c(0, 1)
interval_y <- c(0, 1)

este_densitate(f_xy,interval_x, interval_y)
