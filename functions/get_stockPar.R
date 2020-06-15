


get_stockPar <- function(nh, nm, nl, ab, bb){
  
  # method to randomize alpha and beta pairs based on Brendan's original
  # values.
  
  aval <- c(runif(ns_low, min = ab[1,1], max = ab[1,2]),
            runif(ns_med, min = ab[2,1], max = ab[2,2]),
            runif(ns_high, min = ab[3,1], max = ab[3,2]))
  
  bval <- c(runif(ns_low, min = bb[1,1], max = bb[1,2]),
            runif(ns_med, min = bb[2,1], max = bb[2,2]),
            runif(ns_high, min = bb[3,1], max = bb[3,2]))

  
  # Randomize the pairings of alphas and betas
  alpha <- aval[sample(length(aval))]
  beta <- bval[sample(length(bval))]
  
  return(data.frame(alpha = alpha, beta = beta))
  
}




