


get_initN <- function(alpha, beta){
  
  # Get unfished equilibrium
  N0 <- eq_ricker2(alpha = alpha, beta = beta, U = 0)$R
  
  return(N0)
  
}


