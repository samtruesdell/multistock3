



get_basinSmsy <- function(alpha, beta){
  
  # Potential exploitation rates to check
  exRates <- seq(0, 1, 0.01)
  
  # vector of total basin catch and stock size at various U
  basinC_tmp <- numeric(length(exRates))
  basinS_tmp <- numeric(length(exRates))
  
  # Loop over the potential exploitation rates
  for(i in 1:length(exRates)){
    
    eqRickPar <- eq_ricker(alpha, beta, U = exRates[i])
    
    # Determine what the sum of the equilibrium catch is
    basinC_tmp[i] <- eqRickPar$C
    
    # Determine the equilibrium stock size associated with the catch
    basinS_tmp[i] <- eqRickPar$S
    
  }
  
  # Determine the stock size associated with the maximum catch
  maxCatchIdx <- which.max(basinC_tmp)
  basinSmsy <- basinS_tmp[maxCatchIdx]
  
  return(basinSmsy)
  
}





