

# Function to get the stock parameters based on the number of high
# productivity (nh), medium productivity (nm) and low productivity (nl)
# stocks using a matrix of alpha bounds (ab) and beta bounds (bb)


get_stockPar <- function(nh, nm, nl, ab, bb){
  
  # method to randomize alpha and beta pairs based on Brendan's original
  # values.
  
  aval <- c(runif(nl, min = ab[1,1], max = ab[1,2]),
            runif(nm, min = ab[2,1], max = ab[2,2]),
            runif(nh, min = ab[3,1], max = ab[3,2]))
  
  bval <- c(runif(nl, min = bb[1,1], max = bb[1,2]),
            runif(nm, min = bb[2,1], max = bb[2,2]),
            runif(nh, min = bb[3,1], max = bb[3,2]))

  
  # Randomize the pairings of alphas and betas
  alpha <- aval[sample(length(aval))]
  beta <- bval[sample(length(bval))]
  
  return(data.frame(alpha = alpha, beta = beta))
  
}




