

# Function to estimate the CV of the total production estimate. Based on
# empirical relationship between the number of stocks sampled and the total
# production estimate as sampled in the Kusko:
# 
# alpha = c(5.5,1.9,6.4,2.6,5.8,5.5,2.4,2.1,8.0,3.7,1.7,4.3,2.3)
# beta = c(0.0002986,0.0001986, 0.0003486, 0.001986,
#          0.000168,0.00122,0.000131, 0.00286, 0.0001986, 0.0001986,
#          0.002095,0.000784,0.000911)
#          
# Also see scratch/jones_scalar_cv_est.R
# 
# 8.5 is reasonable value for K and 1000 for b

get_prodCV <- function(K, b, p) {
  
  return ((K / sqrt(p*b)) * (1-p))
  
}


