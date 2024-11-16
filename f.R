grafic_densitate <- function(f, interval = c(-Inf,Inf))
{
  a <- -1000
  b <- 1000
  if(interval[1] != -Inf)
    a <- interval[1]
  if(interval[2] != Inf)
    b <- interval[2] 
  xs <- seq(a,b, length.out = 1000)
  ys <- c()
  for (x in xs) {
    ys = append(ys, f(x))
  }
  plot(xs, ys, type="l", main = "Densitatea functiei" ,col="red", xlab="x", ylab="y")
}

grafic_repartitie <- function(f,interval = c(-Inf,Inf))
{
  a <- -1000
  b <- 1000
  if(interval[1] != -Inf)
    a <- interval[1]
  if(interval[2] != Inf)
    b <- interval[2]
  xs <- seq(a,b, length.out = 1000)
  ys <- c()
  for (x in xs) {
    ys = append(ys, repartitie(f,interval,x))
  }
  plot(xs, ys, type="l", main = "Repartitia functiei", col="blue", xlab="x", ylab="y")
}

library(scatterplot3d)

grafic_densitate_bidimensional <- function(f,interval_x = c(-Inf,Inf),interval_y = c(-Inf,Inf))
{
  a <- -100
  b <- 100
  c <- -100
  d <- 100
  if(interval_x[1] != -Inf)
    a <- interval_x[1]
  if(interval_x[2] != Inf)
    b <- interval_x[2] 
  if(interval_y[1] != -Inf)
    c <- interval_y[1]
  if(interval_y[2] != Inf)
    d <- interval_y[2] 
  xs <- seq(a,b, length.out = 100)
  ys <- seq(c,d, length.out = 100)
  zs <- matrix(nrow = 100, ncol = 100)
  for (i in 1:100) 
    for(j in 1:100)
      zs[i, j] = f(xs[i],ys[j])
  persp(xs,ys,zs)
}

grafic_repartitie_bidimensional <- function(f,interval_x = c(-Inf,Inf),interval_y = c(-Inf,Inf))
{
  a <- -100
  b <- 100
  c <- -100
  d <- 100
  if(interval_x[1] != -Inf)
    a <- interval_x[1]
  if(interval_x[2] != Inf)
    b <- interval_x[2] 
  if(interval_y[1] != -Inf)
    c <- interval_y[1]
  if(interval_y[2] != Inf)
    d <- interval_y[2] 
  xs <- seq(a,b, length.out = 100)
  ys <- seq(c,d, length.out = 100)
  zs <- matrix(nrow = 100, ncol = 100)
  for (i in 1:100) 
    for(j in 1:100)
      zs[i, j] = repartitie_bidimensional(f,interval_x,interval_y,xs[i],ys[j])
  persp(xs,ys,zs)
}

grafic_densitate(f)
grafic_repartitie(f, interval)
grafic_densitate_bidimensional(f_xy)
grafic_repartitie_bidimensional(f_xy,interval_x,interval_y)

f <- function(x)(x/2)
interval <- c(0,2)
f_xy <- function (x, y) {
  return (3/2 * (x^2+y^2))
}
interval_x = c(0,1)
interval_y = c(0,1)
