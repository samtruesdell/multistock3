

# load libraries
source('processes/loadLibs.R')

# load in paramter values
source('parameters/msPar.R')



# load the functions
funLst <- list.files('functions', full.names = TRUE)
sapply(funLst, source)


# Calculate some paramters
nage <- length(pReturn)
ns <- ns_high + ns_med + ns_low
# Spin up n years before running the model
nySpinFit <- nySpin + nyFit

# build the containers
source('processes/get_arrays.R')

# Get stock parameters
stpar <- get_stockPar(nh = ns_high, nm = ns_med, nl = ns_low, 
                      ab = abounds, bb = bbounds)

# Get equilibrium stock size
N0 <- get_initN(alpha = stpar$alpha, beta = stpar$beta)
N[1:nage,1:nage,] <- sapply(N0, function(x) rep(x * pReturn, each = nage))

# Get equilibrium initial recruitment
R[1:nage,] <- rep(N0, each = nage)

# Include zeros to avoid warning messages later
Stot[1:nage,] <- 0
Ctot[1:nage,] <- 0

# Run a loop to get the true stock dynamics
for(y in (nage+1):nySpinFit){
  
  # Harvest rate for year y
  UTemp <- rtnorm(1, mean = initUMean, sd = initUSD, lower = 0)
  
  # Loop over stocks
  for(s in 1:ns){
    
    # Calculate the run size
    Run[y,,s] <- get_raa(R = R[,s], yIdx = y, pRet = pReturn)
    
    # Escapement-at-age and total escapement
    S[y,,s] <- Run[y,,s] * (1 - UTemp)
    Stot[y,s] <- sum(S[y,,s])
    
    # Catch-at-age and total catch
    C[y,,s] <- Run[y,,s] * UTemp
    Ctot[y,s] <- sum(C[y,,s])
    
    # Recruits spawned in year y
    R[y,s] <- ricker(alpha = stpar$alpha[s], beta = stpar$beta[s], 
                     S = sum(S[y,,s]))
  } # close s
} # close y

# Add in the uncertainty to the catch and the escapement observations
# Table to relate stock to which type of CV (all weirs now...)
cvTab <- tibble(
  s = 1:ns,
  cvIdx = 2)

# Add observation errors
for(y in (nage+1):nySpinFit){
  
  # Loop over stocks
  for(s in 1:ns){
    
    # Escapement and catch totals observed with error
    Stot_oe[y,s] <- rtnorm(1, Stot[y,s], cvAW[cvTab$cvIdx[s]])
    Ctot_oe[y,s] <- rtnorm(1, Ctot[y,s], cvAW[cvTab$cvIdx[s]])
    
    # Escapement proportions-at-age observed with error (assumes that catch
    # and escapement are estimated from the same samples)
    paa_oeNTemp <- c(rmultinom(n = 1, size = oe_paaS, prob = S[y,,s]))
    paa_oe[y,,s] <- paa_oeNTemp / sum(paa_oeNTemp)
  
  } # close s
} # close y

# Calculate the observed run size
for(s in 1:ns){
  runObs <- Stot_oe[,s] + Ctot_oe[,s]
  runHat[,,s] <-  paa_oe[,,s] * runObs
} # close s

## Math needs to be checked here...........
# Calculate the observed recruitment
for(y in (nage+1):(nySpinFit-nage)){
  for(s in 1:ns){
    
    # Observed recruitment associated with year y is the sum of the diagonal 
    # of the run-at-age for the next nage-1 years
    R_oe[y,s] <-  sum(diag(runHat[y:(y+nage-1),,s]))

  } # close s
} # close y

# Fit the assessment model

#  something is up with the calcs ...
plot(R_oe[,1] ~ Stot_oe[,1])

x <- 1:2500
y <- ricker(alpha = stpar$alpha[s], beta = stpar$beta[s], 
       S = x)
plot(x,y)

