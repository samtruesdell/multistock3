# Staton function to help in the calculation of Smsy reference point
# for multiple populations.
# function to get BRPs
eq_ricker = function(alpha, beta, U) {
  # fished equilibrium states for each substock
  Req_u = log(alpha * (1 - U)) / (beta * (1 - U))
  Req_u = ifelse(Req_u < 0, 0, Req_u)
  Seq_u = Req_u * (1 - U)
  Seq_u = ifelse(Seq_u < 0, 0, Seq_u)
  Ceq_u = Req_u * U
  Ceq_u = ifelse(Ceq_u < 0, 0, Ceq_u)
  
  # sum across substocks
  Seq_tot_u = sum(Seq_u)
  Ceq_tot_u = sum(Ceq_u)
  
  # unfished equilibrium recruitment
  Req = (log(alpha) / beta)
  
  # approximation to Smsy
  Smsy_sum = sum(Req * (0.5 - 0.07 * log(alpha)))
  
  # output
  list(S = Seq_tot_u, C = Ceq_tot_u, Smsy_sum = Smsy_sum)
}
