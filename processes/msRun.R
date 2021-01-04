

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

# Get the stocks to sample ----- change to vary within loop --------
stocks2sample <- 1:2

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

# Get equilibrium run
for(s in 1:ns){
  Run[1:nage,,s] <- rep(rev(R[1:(nage),s] * rev(pReturn)), each = nage)
}


# Initial escapement-at-age and total escapement (0.25 initial U)
S[1:nage,,] <- Run[1:nage,,] * (1 - 0.25)
Stot[1:nage,] <- apply(S[1:nage,,], c(1,3), sum)

# Initial catch-at-age and total catch
C[1:nage,,] <- Run[1:nage,,] * 0.25
Ctot[1:nage,] <- apply(C[1:nage,,], c(1,3), sum)

# Initial observed run size (no observation error...)
run_oe[1:nage,,] <-  Run[1:nage,,]

# Initial observed recruitment
for(s in 1:ns){
  R_oe[1:nage,s] <-  sum(diag(run_oe[(1:(nage)),,s]))
}

# Initial observed total stock size (no observation error...)
Stot_oe[1:nage,] <- Stot[1:nage,]



# Table to relate stock to which type of CV (all weirs now...)
cvTab <- tibble(
  s = 1:ns,
  cvIdx = 2)

# Initially turn on flag to update escapement goal
updateEGFlag <- TRUE

# # Set Smsy & EG for initial years before it is calculated
Smsy[1:(nage+1)] <- sum(N0)
# EG[1:(nySRMod + nage)] <- Smsy[1:(nySRMod + nage)]


# Grid of options
opt <- expand.grid(n = 1:nrep,
                   egs = egscalar)


# Loop over options
for(i in 1:nrow(opt)){

  # Loop over years
  for(y in (nage+1):nySpinFit){
    
    # Get the run size so a run estimate can be calculated
    for(s in 1:ns){
      
      # Calculate the run size ... check math
      # Run[y,,s] <- get_raa(R = R[,s], yIdx = y, pRet = pReturn)
      Run[y,,s] <- rev(R[(y-nage):(y-1),s] * rev(pReturn))
      
    }
    
    runEst[y] <- rtnorm(1, mean = sum(apply(Run[y,,], 1, sum)), 
                        sd = oe_runEst, lower = 0)
    
    
    # # Harvest rate for year y ----- update to advice ------
    # UTemp <- rtnorm(1, mean = initUMean, sd = initUSD, lower = 0, upper = 1)
    
    if(y < (nySRMod + nage)){
      U <- rtnorm(1, mean = initUMean, sd = initUSD, lower = 0, upper = 1)
    }else{
      if(runEst[y] >= EG[y]){
        U <- (runEst[y] - EG[y]) / runEst[y]
      }else{
        U <- 0
      }
    }
    
    # Loop over stocks
    for(s in 1:ns){
      
      # Escapement-at-age and total escapement
      S[y,,s] <- Run[y,,s] * (1 - U)
      Stot[y,s] <- sum(S[y,,s])
      
      # Catch-at-age and total catch
      C[y,,s] <- Run[y,,s] * U
      Ctot[y,s] <- sum(C[y,,s])
      
      # Recruits spawned in year y
      R[y,s] <- ricker(alpha = stpar$alpha[s], beta = stpar$beta[s], 
                       S = Stot[y,s])
      
      
      # Add observation errors
      
      # Escapement and catch totals observed with error (should possibly be lognormal?)
      Stot_oe[y,s] <- rtnorm(1, Stot[y,s], cvAW[cvTab$cvIdx[s]])
      Ctot_oe[y,s] <- rtnorm(1, Ctot[y,s], cvAW[cvTab$cvIdx[s]])
      
      # Escapement proportions-at-age observed with error (assumes that catch
      # and escapement are estimated from the same samples)
      paa_oeNTemp <- c(rmultinom(n = 1, size = oe_paaS, prob = S[y,,s]))
      paa_oe[y,,s] <- paa_oeNTemp / sum(paa_oeNTemp)
      
      # Calculate the observed run size
      run_oe[y,,s] <-  paa_oe[y,,s] * (Stot_oe[y,s] + Ctot_oe[y,s])
      
      
      # Calculate the observed recruitment
      
      # Observed recruitment associated with year y is the sum of the diagonal 
      # of the run-at-age for the next nage-1 years
      # R_oe[y,s] <-  sum(diag(run_oe[(y + 1):(y + nage),,s]))
      R_oe[y-nage,s] <-  sum(diag(run_oe[(y -nage + 1):y,,s]))
      
      
      
    } # close s
    
    
    # Fit the assessment model
    
    if(y > nySRMod + nage & updateEGFlag){
      
      # Get the appropriate number of years used to fit the assessment model
      # (from the end of the data set)
      R_lm <- apply(R_oe[(y-(nySRMod-1)):(y-nage),stocks2sample], 1, sum)
      S_lm <- apply(Stot_oe[(y-(nySRMod-1)):(y-nage),stocks2sample], 1, sum)
      
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
      rMod[[y]] <- list(R_lm = R_lm, S_lm = S_lm,
                        aprime = aprime, bprime = bprime)
    
      # Calculate Smsy
      # Smsy[y,s] <- bprime * (0.5 - 0.07 * aprime)
      Smsy[y+1] <- log(aprime) / bprime * (0.5 - 0.07 * log(aprime))
      
      updateEGFlag <- ifelse(updateEG, TRUE, FALSE)
      
    }else{
      
      Smsy[y+1] <- Smsy[y]
  
    }
    
    EG[y+1] <- Smsy[y] * opt$egs[i]  # 3 is placeholder ... will have another loop
    
    
    
  
    
  } # close y
  
  
} # close i (options loop)












