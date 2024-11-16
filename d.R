# am definit o clasa care contine mai multe proprietati ale variabilei aleatoare

construieste_obiect <- function(f, interval_x = c(-Inf,Inf), interval_y = c(-Inf,Inf), dimensiune = 1) {
  if (dimensiune == 1) {
    pdf <- function(x) {
      f(x)
    }
    cdf <- function(x) {
      repartitie(f,interval_x,x)
    }
    media <- E(f, interval_x)
    varianta <- Var(f,interval_x)
  } else if (dimensiune == 2) {
    pdf <- function(x, y) {
      f(x, y)
    }
    cdf <- function(x, y) {
      repartitie(f,interval_x, interval_y, x, y)
    }
    media <- E_bidimensional(f,interval_x,inerval_y)
    varianta <- Var_bidimensional(f,interval_x,interval_y)
  } else {
    stop("Dimensiunea variabilei poate fi doar 1 sau 2")
  }
  rezultat <- list(
    pdf = pdf,
    cdf = cdf,
    media = media,
    varianta = varianta
  )
  
  class(rezultat) <- "V.A. Continua"
  
  return(rezultat)
}

