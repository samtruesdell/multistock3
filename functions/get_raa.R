

# Function to calculate the run based on the recruitment at previous
# ages



get_raa <- function(R, yIdx, pRet){
  
 
  raa <-  rev(R[(yIdx-nage):(yIdx-1)] * rev(pRet[1:nage]))
  
  return(raa)
  
}



