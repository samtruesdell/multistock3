

# Based on Staton function to help in the calculation of Smsy reference point

eq_ricker2 <- function(alpha, beta, U){
  
  # fished equilibrium states for each substock
  Req_u = log(alpha * (1 - U)) / (beta * (1 - U))
  Req_u = ifelse(Req_u < 0, 0, Req_u)
  Seq_u = Req_u * (1 - U)
  Seq_u = ifelse(Seq_u < 0, 0, Seq_u)
  Ceq_u = Req_u * U
  Ceq_u = ifelse(Ceq_u < 0, 0, Ceq_u)
  
  # unfished equilibrium recruitment
  Req = (log(alpha) / beta)
  
  # approximation to Smsy
  Smsy = Req * (0.5 - 0.07 * log(alpha))
  
  # output
  return(list(R = Req_u, S = Seq_u, C = Ceq_u, Smsy_sum = Smsy))
}




