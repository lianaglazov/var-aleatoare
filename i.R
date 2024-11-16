#functia de repartitie P(X <= x)
repartitie <- function(f,interval = c(-Inf,Inf),x)
{
  if(interval[1] > x)
    f_rep <- 0
  else if(interval[2] < x)
    f_rep <- 1
  else 
      f_rep <- integrate(f,interval[1],x)$value
  return(f_rep)
}

# functia de repartitie pentru variabila bidimensionala P(X <= x, Y <= y)
repartitie_bidimensional <- function(f, interval_x=c(-Inf,Inf),interval_y=c(-Inf,Inf),x,y)
{
  if(interval_x[1] > x || interval_y[1] > y)
    f_rep <- 0
  else 
    f_rep <- integrate(function(y) {
      sapply(y, function(y_val) {
        integrate(function(x) f(x, y_val), interval_x[1], x)$value
      })
    }, interval_y[1], y)$value
  return(f_rep)
}

P_1 <- function(f, interval = c(-Inf,Inf), expresie)
{
  expresie <- unlist(strsplit(expresie, " "))
  op <- expresie[2]
  if(length(expresie) != 3 ||!switch(op, "="=1, ">="=1, ">"=1,"<="=1, "<"=1,0))
    return("Expresia a fost scrisa gresit!")
  x <- as.numeric(expresie[3])
  f_rep <- repartitie(f,interval,x)
  rez <- switch(op,
                "=" = 0,
                "<=" = f_rep,
                "<" = f_rep,
                ">=" = 1 - f_rep,
                ">" = 1 - f_rep)
  return(rez)
}

P <- function(f, interval = c(-Inf,Inf), expresie)
{
  expresie <- unlist(strsplit(expresie, " "))
  #probabilitate conditionata
  if(length(expresie) == 3)
    return(P_1(f,interval,expresie))
  if(length(expresie) != 7)
    return("Expresia a fost scrisa gresit")
  if(expresie[4] == "|" )
  {
    exp1 <- paste(expresie[1:3], collapse = " ")
    p1 <- P_1(f,interval,exp1)
    exp2 <- paste(expresie[5:7], collapse = " ")
    p2 <- P_1(f,interval,exp2)
    op1 <- expresie[2]
    x1 <- as.numeric(expresie[3])
    rep1 <- repartitie(f, interval, x1)
    op2 <- expresie[6]
    x2 <- as.numeric(expresie[7])
    rep2 <- repartitie(f, interval, x2)
    if(p1 == 0)
      return(0)
    if(p2 == 0)
      return("Probabilitatea expresiei la care conditionam este 0")
    
    #avem urmatoarele cazuri
    
    #acelasi semn -> intersectia este intervalul cel mai mic
    if(op1 %in% c(">", ">=") && op2 %in% c(">", ">="))
      if(x1 > x2)
        return(p1/p2)
      else return(1)
    if(op1 %in% c("<", "<=") && op2 %in% c("<", "<="))
      if(x1 < x2)
        return(p1/p2)
    else return(1)
    
    #caz in care intersectia este multimea vida
    if (op1 %in% c("<=","<") && op2 %in% c(">=", ">") && x1 <= x2)
      return (0);
    if (op1 %in% c(">=",">") && op2 %in% c("<=", "<") && x1 >= x2)
      return (0);
    
    #in rest facem intersectia intervalelor
    #P(a<X<b) = P(X<b) - P(X<a)
    if(x1>x2)
      return((rep1-rep2)/rep2)
    else
      return((rep2-rep1)/rep2)
  }
  else
    return(P_1(f,interval,expresie))
    
}

paste(unlist(strsplit("x > 3 | x < 2", " "))[5:7], collapse = " ")

f <- function(x)(dunif(x))
interval <- c(-Inf,Inf)
P(f,interval, "X > 0.6")

 f <- function (x) {
  fun <- 0.1*(3*(x^2) + 1)
  return ( fun )
 }
repartitie(f,c(0,2),1)
P(g,interval,"x > 1 | x < 1.5")
 
