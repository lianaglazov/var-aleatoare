Eg <- function(f, g, interval= c(-Inf,Inf))
{
  h <- function(x)
  {
    g(x)*f(x)
  }
  return(integrate(h,interval[1], interval[2])$value)
}

Varg <- function(f,g,interval= c(-Inf,Inf))
{
  m <- Eg(f,g,interval)
  h <- function(x)
  {
    (g(x) - m)^2*f(x)
  }
  return(integrate(h,interval[1], interval[2])$value)
}

Eg_x_y <- function(f_xy, g_xy, interval_x = c(-Inf,Inf), interval_y = c(-Inf,Inf))
{
  h <- function(x,y)
  {
    g_xy(x,y) * f_xy(x,y)
  }
  return(integrate(function(x) {
    sapply(x, function(x_val) {
       integrate(function(y) h(x_val, y), interval_y[1], interval_y[2])$value
    })
  }, interval_x[1], interval_x[2])$value)
}

Varg_x_y <- function(f_xy,g_xy,interval_x = c(-Inf,Inf), interval_y = c(-Inf,Inf))
{
  m <- Eg_x_y(f_xy,g_xy,interval_x, interval_y)
  h <- function(x,y)
  {
    (g_xy(x,y) - m)^2*f(x,y)
  }
  return(integrate(function(x) {
    sapply(x, function(x_val) {
      integrate(function(y) h(x_val, y), interval_y[1], interval_y[2])$value
    })
  }, interval_x[1], interval_x[2])$value)
}

f1 <- function(x)(x^2)
f2 <- function(x) (exp(1)^(-1 * x))

Eg(f1,f2, c(0,1))
Varg(f1,f2,c(0,1))
f_xy <- function(x, y) {
  (1/(2*pi)) * exp(-((x^2 + y^2)/2))
}

g_xy <- function(x, y) {
  x * y
}

interval_x <- c(-Inf, Inf)
interval_y <- c(-Inf, Inf)

Eg_x_y(f_xy, g_xy, interval_x, interval_y)
Varg_x_y(f_xy, g_xy, interval_x, interval_y)

