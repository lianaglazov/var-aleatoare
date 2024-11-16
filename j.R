Cov <- function(f,interval_x = c(-Inf,Inf),interval_y = c(-Inf,Inf))
{
  g <- function(x,y) (x*y)
  m_xy <- Eg_x_y(f,g,interval_x,interval_y)
  m_x <- E_bidimensional(f, interval_x,interval_y)$E_X
  m_y <- E_bidimensional(f, interval_x,interval_y)$E_Y
  return(m_xy - m_x*m_y)
}

Cor <- function(f,interval_x = c(-Inf, Inf),interval_y = c(-Inf,Inf))
{
  c <- Cov(f,interval_x,interval_y)
  v_x <- Var_bidimensional(f, interval_x, interval_y)$Var_x
  v_y <- Var_bidimensional(f, interval_x, interval_y)$Var_y
  return(c/sqrt(v_x*v_y))
}

#exemple

Cov(f_xy,interval_x,interval_y)
Cor(f_xy,interval_x,interval_y)

