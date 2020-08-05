

# load libraries
source('processes/loadLibs.R')

# load in paramter values
source('parameters/msPar.R')



# load the functions
funLst <- list.files('functions', full.names = TRUE)
sapply(funLst, source)


# Calculate some variables to use later
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

# Run a loop to spin up
for(y in (nage+1):nySpinFit){
  
  # Harvest rate for year y
  UTemp <- rtnorm(1, mean = initUMean, sd = initUSD, lower = 0, upper = 1)
  
  # Loop over stocks
  for(s in 1:ns){
    
    # Calculate the run size ... check math
    # Run[y,,s] <- get_raa(R = R[,s], yIdx = y, pRet = pReturn)
    Run[y,,s] <- rev(R[(y-nage):(y-1),s] * rev(pReturn))
    
    # Escapement-at-age and total escapement
    S[y,,s] <- Run[y,,s] * (1 - UTemp)
    Stot[y,s] <- sum(S[y,,s])
    
    # Catch-at-age and total catch
    C[y,,s] <- Run[y,,s] * UTemp
    Ctot[y,s] <- sum(C[y,,s])
    
    # Recruits spawned in year y
    R[y,s] <- ricker(alpha = stpar$alpha[s], beta = stpar$beta[s], 
                     S = Stot[y,s])
    
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
  run_oe[,,s] <-  paa_oe[,,s] * (Stot_oe[,s] + Ctot_oe[,s])
} # close s

## Math needs to be checked here...........
# Calculate the observed recruitment
for(y in (nage+1):(nySpinFit-nage)){
  for(s in 1:ns){
    
    # Observed recruitment associated with year y is the sum of the diagonal 
    # of the run-at-age for the next nage-1 years
    R_oe[y,s] <-  sum(diag(run_oe[(y + 1):(y + nage),,s]))

  } # close s
} # close y

# Fit the assessment model
for(s in 1:ns){
  
  # Get the appropriate number of years used to fit the assessment model
  # (from the end of the data set)
  R_lm <- tail(R_oe[,s], sryrs)
  S_lm <- tail(Stot_oe[,s], sryrs)
  
  # Calculate the parameters of the Ricker model
  lnRS <- log(R_lm+1e-5) - log(S_lm+1e-5)
  SRlm <- try(lm(lnRS ~ S_lm))
  lRpar <- coef(SRlm)
  abase <- lRpar[1]
  bbase <- -lRpar[1] / lRpar[2]
  abase <- exp(lRpar[1])
  bbase <- -lRpar[2]
  
  # get unbiased estimates (H&W p. 269)
  sdR <- summary(SRlm)$sigma
  aprime <- abase + sdR^2/2
  bprime <- aprime / abase * bbase
  Rpar <- c(aprime, bprime)
  
  # Calculate Smsy
  Smsy <- Rpar[2] * (0.5 - 0.07 * Rpar[1])
  
}



# 
# #  something is up with the calcs ... R_oe looks like
# plot(R_lm ~ S_lm)
# 
# x <- 1:max(S_lm)
# y <- ricker(alpha = stpar$alpha[s], beta = stpar$beta[s], 
#        S = x)
# yprime <- ricker(alpha = aprime, beta = bprime, 
#                  S = x)
# lines(x,y, col = 'red')
# lines(x,yprime, col = 'blue')
# 
