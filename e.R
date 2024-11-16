densitati_marg_cond <- function(f, interval_x = c(-Inf,Inf), interval_y = c(-Inf,Inf)) {
  # densitatile marginale
  fx <- function(x) {
    integrate(function(y) f(x, y), interval_y[1], interval_y[2])$value
  }
  
  fy <- function(y) {
    integrate(function(x) f(x, y), interval_x[1], interval_x[2])$value
  }
  
  # densitatile conditionate
  x_cond_y <- function(x, y) {
    f(x, y) / fy(y)
  }
  y_cond_x <- function(y, x) {
    f(x, y) / fx(x)
  }
  
  return(list(fx = fx, fy = fy, x_cond_y = x_cond_y,y_cond_x = y_cond_x))
}

# exemplu
f <- function(x, y) {
  dnorm(x) * dnorm(y)
}
interval_x <- c(-Inf, 6)
interval_y <- c(3, 9)

result <- densitati_marg_cond(f, interval_x, interval_y)

x_marg <- result$fx
x_marg(2)
# rezultatul densitatii conditionate a lui y la x=1, y = 0
y_cond_x <- result$y_cond_x

