

# load libraries
source('processes/loadLibs.R')

# load in paramter values
source('processes/msPar.R')



# load the functions
funLst <- list.files('functions', full.names = TRUE)
sapply(funLst, source)


# Calculate some paramters
nage <- ageMax
ns <- ns_high + ns_med + ns_low

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

# Spin up n years before running the model
nySpinFit <- nySpin + nyFit
for(y in (nage+1):nySpinFit){
  UTemp <- rtnorm(1, mean = initUMean, sd = initUSD, lower = 0)
  for(s in 1:ns){
    Run[y,,s] <- get_raa(R = R[,s], yIdx = y, pRet = pReturn)
    S[y,,s] <- Run[y,,s] * (1 - UTemp)
    Stot[y,s] <- sum(S[y,,s])
    C[y,,s] <- Run[y,,s] * UTemp
    Ctot[y,s] <- sum(C[y,,s])
    R[y,s] <- ricker(alpha = stpar$alpha[s], beta = stpar$beta[s], 
                     S = sum(S[y,,s]))
  }
}

# Add in the uncertainty to the catch and the escapement observations

# Table to relate stock to which type of CV (all weirs now...)
cvTab <- tibble(
  s = 1:ns,
  cvIdx = 2)

for(y in (nage+1):nySpinFit){
  for(s in 1:ns){
    Stot_oe[y,s] <- rtnorm(1, Stot[y,s], cvAW[cvTab$cvIdx[s]])
    Ctot_oe[y,s] <- rtnorm(1, Ctot[y,s], cvAW[cvTab$cvIdx[s]])
    paa_oeNTemp <- c(rmultinom(n = 1, size = oe_paaS, prob = S[y,,s]))
    paa_oe[y,,s] <- paa_oeNTemp / sum(paa_oeNTemp)
  }
}

for(y in (nage+1):(nySpinFit-nage)){
  for(s in 1:ns){
    Rhat[y,s] <- diag(paa_oe[y:(y+nage),,s]) %*% 
      (Stot_oe[y:(y+nage),s] + Ctot_oe[y:(y+nage),s])
  }
}

# Fit the assessment model

R[y] <- diag(srdat$paaB_e[(y+4):(y+7),]) %*% # estimated PAA
  (srdat$SB_e[(y+4):(y+7)] +           # sum of escapement and
     srdat$HB_e[(y+4):(y+7)])          # catch is total recruits




