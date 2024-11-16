E <- function(f, interval = c(-Inf,Inf))
{
  g <- function(x)
  {
    x*f(x)
  }
  
  return(integrate(g,interval[1], interval[2])$value)
}

moment <- function(ord, f, interval = c(-Inf,Inf))
  {
    f_ord <- function(x)
    {
      x^ord*f(x)
    }
    return(integrate(f_ord,interval[1], interval[2])$value)
  }

moment_centrat <- function(ord, f, interval = c(-Inf,Inf))
{
  m <-  E(f, interval)
  f_ord <- function(x)
  {
    (x - m)^ord * f(x)
  }
  return(integrate(f_ord, interval[1], interval[2])$value)
}

Var <- function(f, interval = c(-Inf,Inf))
{
  g <- function(x)
  {
    x^2*f(x)
  }
  return(integrate(g,interval[1], interval[2])$value-E(f,interval)^2)
}

E_bidimensional <- function(f,interval_x = c(-Inf,Inf), interval_y = c(-Inf,Inf)) {
  
  E_X <-  integrate(function(x) {
    sapply(x, function(x_val) {
      x_val * integrate(function(y) f(x_val, y), interval_y[1], interval_y[2])$value
    })
  }, interval_x[1], interval_x[2])$value
  
  E_Y <- integrate(function(y) {
    sapply(y, function(y_val) {
      y_val * integrate(function(x) f(x, y_val), interval_x[1], interval_x[2])$value
    })
  }, interval_y[1], interval_y[2])$value
  
  return(list(E_X = E_X, E_Y = E_Y))
}

Var_bidimensional <- function(f,interval_x = c(-Inf,Inf), interval_y = c(-Inf,Inf)){
  E <- E_bidimensional(f_xy,interval_x, interval_y)
  E2_X <-  integrate(function(x) {
    sapply(x, function(x_val) {
      x_val^2 * integrate(function(y) f(x_val, y), interval_y[1], interval_y[2])$value
    })
  }, interval_x[1], interval_x[2])$value
  E2_Y <- integrate(function(y) {
    sapply(y, function(y_val) {
      y_val^2 * integrate(function(x) f(x, y_val), interval_x[1], interval_x[2])$value
    })
  }, interval_y[1], interval_y[2])$value
  return(list(Var_x = E2_X - E$E_X, Var_y = E2_Y - E$E_Y))
}

moment_bidimenisonal <- function(ord, f, interval_x = c(-Inf,Inf), interval_y = c(-Inf,Inf))
{
  f_ord <- function(x,y)
  {
    x^ord*f(x,y)
  }
  mom_x <-  integrate(function(x) {
    sapply(x, function(x_val) {
      integrate(function(y) f_ord(x_val, y), interval_y[1], interval_y[2])$value
    })
  }, interval_x[1], interval_x[2])$value
  f_ord <- function(x,y)
  {
    y^ord*f(x,y)
  }
  mom_y <- integrate(function(y) {
    sapply(y, function(y_val) {
      integrate(function(x) f_ord(x, y_val), interval_x[1], interval_x[2])$value
    })
  }, interval_y[1], interval_y[2])$value
  return(list(mom_x = mom_x, mom_y = mom_y))
}

moment_centrat_bidimenisonal <- function(ord, f, interval_x = c(-Inf,Inf), interval_y = c(-Inf,Inf))
{
  m <- E_bidimensional(f,interval_x,interval_y)$E_X
  f_ord <- function(x,y)
  {
    (x-m)^ord*f(x,y)
  }
  mom_x <-  integrate(function(x) {
    sapply(x, function(x_val) {
      integrate(function(y) f_ord(x_val, y), interval_y[1], interval_y[2])$value
    })
  }, interval_x[1], interval_x[2])$value
  m <- E_bidimensional(f,interval_x,interval_y)$E_Y
  f_ord <- function(x,y)
  {
    (y-m)^ord*f(x,y)
  }
  mom_y <- integrate(function(y) {
    sapply(y, function(y_val) {
      integrate(function(x) f_ord(x, y_val), interval_x[1], interval_x[2])$value
    })
  }, interval_y[1], interval_y[2])$value
  return(list(mom_x = mom_x, mom_y = mom_y))
}

f_xy <- function(x, y) {
  return((1/(2*pi)) * exp(-(x^2 + y^2)/2))
}

interval_x <- c(-Inf, Inf)
interval_y <- c(-Inf, Inf)

moment_centrat_bidimenisonal(3,f_xy)

f <- function(x)
{
  x^3
}

interval <- c(0,1)

Var(f,interval)
x <- E(f,interval)
moment_centrat(2,f,interval)
moment(3,f,interval)
