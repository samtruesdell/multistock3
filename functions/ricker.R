
# Ricker equation

ricker <- function(alpha, beta, S){
  
  R <- alpha * S * exp(-beta * S)
  
  return(R)
  
}




