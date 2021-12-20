

# Function to get the stock parameters based on the number of high
# productivity (nh), medium productivity (nm) and low productivity (nl)
# stocks using a matrix of alpha bounds (ab) and beta bounds (bb)


get_stockPar <- function(nh, nm, nl, rickPars){
  
  vcov_mat <- cov(rickPars)

  
  # Means for alpha and beta
  m_alpha <- mean(rickPars$alpha)
  m_beta <- mean(rickPars$beta)

  #Generate lots of draws from the mv-norm distn
  a_b_sample <- rtmvnorm(1000, 
                        mean = c(m_alpha, m_beta), 
                        sigma = vcov_mat,
                        lower = c(1.0, -Inf) # exclude alpha < 1
                        ) %>%
    magrittr::set_colnames(c('alpha', 'beta'))
  
  # Censor negative values of alpha and beta
  trunc_sample <- subset(a_b_sample, a_b_sample[,1] > 0 & a_b_sample[,2] > 0)
  
  # create three sub-samples of alpha, beta pairs for low, med and high 
  # productivity stocks, using quantiles
  prod_cuts <- quantile(trunc_sample[,1], c(.33,.67))
  
  low_prod <- subset(trunc_sample, trunc_sample[,1] < prod_cuts[1])
  high_prod <- subset(trunc_sample,trunc_sample[,1] > prod_cuts[2])
  med_prod <- subset(trunc_sample,trunc_sample[,1] > prod_cuts[1] & 
                       trunc_sample[,1] < prod_cuts[2])
  
  # Subset for the number of low-med-high alpha/beta pairs
  lowpairs <- low_prod[sample(length(low_prod[,1]),nl,replace=TRUE),] %>%
    as_tibble()
  medpairs <- med_prod[sample(length(med_prod[,1]),nm,replace=TRUE),] %>%
    as_tibble()
  highpairs <- high_prod[sample(length(high_prod[,1]),nh,replace=TRUE),] %>%
    as_tibble()
  
  # Create a single object for output
  ret <- bind_rows(lowpairs, medpairs, highpairs)
  
  return(ret)
  
}




