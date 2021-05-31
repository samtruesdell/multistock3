

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

# updated number of years to include spin up
ny2 <- ny + nySpin + nage
# Spin up n years before running the model
# nySpinFit <- nySpin + nyFit

# Get the stocks to sample (remove first row when no stocks sampled)
# stocks2sample <- expand.grid(replicate(ns, 0:1, simplify = FALSE))[-1,]
# names(stocks2sample) <- paste0('stock', 1:ns)

# Determine sampling scheme -- matrix is 0 for unsampled and CV otherwise
sampDsn <- expand.grid(replicate(ns, c(0,cvAW), simplify = FALSE))


# Remove cases where there are no weirs sampled
# removeIdx <- apply(sampDsn, 1, sum) %% cvAW[1] == 0
removeIdx <- apply(sampDsn, 1, function(x) length(x[x==cvAW[2]])) == 0
sampDsn <- sampDsn[!removeIdx,]
names(sampDsn) <- paste0('stock', 1:ns)



# build the containers
source('processes/get_arrays.R')


# Determine alpha and beta values for each of the reps
stpar <- list()
for(i in 1:nrep){
  stpar[[i]] <- get_stockPar(nh = ns_high, nm = ns_med, nl = ns_low, 
                             ab = abounds, bb = bbounds)
}



# Subsample the design
sampDsn2 <- sampDsn

## Assign weir values to 100 to help identify unique types of sampling
sampDsn2[sampDsn2 == cvAW[1]] <- 100
rs <- apply(sampDsn2, 1, sum) # really there are length(unique(rs)) types

## Find the unique types of weir/aerial combinations
uOpt <- unique(rs)

## Identify index numbers in design matrix that apply to each sample type
## (this is independent of any productivity stratification)
rowNumLst <- lapply(uOpt, function(x) which(rs == x))

## Sample the indices for each sampling type (replace = TRUE!!)
## Would rather not use replacement but I think it just doesn't matter.
sampDsnIdx <- unlist(lapply(rowNumLst, 
                            function(x){
                              x[sample(length(x), size = nrep, replace = TRUE)]
                            }))

# Create a new design matrix
sampDsn <- sampDsn[sampDsnIdx,]

# Grid of options. Repeating options that were sampled before ... but does it 
# even matter if they are equivalent???
opt <- expand.grid(n = 1,#:nrep, ... nrep in the dsn matrix??
                   s2s = 1:nrow(sampDsn),
                   egs = egscalar)


# opt$u <- rs[match(1:length(rs), opt$s2s)]

# uScen <- unique(opt[,2:3])

nopt <- nrow(opt)

source('processes/get_arraysSummary.R')

# Print out the number of iterations
cat('number of iterations:', nrow(opt), '\n')

# Loop over options
for(i in 1:nopt){

  tmp_stpar <- stpar[[opt$n[i]]]
  tmp_alpha <- tmp_stpar$alpha
  tmp_beta <- tmp_stpar$beta
  
  # Get equilibrium stock size
  N0 <- get_initN(alpha = tmp_alpha, beta = tmp_beta)
  N[1:nage,1:nage,] <- sapply(N0, function(x) rep(x * pReturn, each = nage))
  
  # Get true Smsys. Though this is "sum" each entry is one stock ...
  # wanting to calculate individually for use below.
  Smsy_true <- sapply(1:nrow(tmp_stpar), 
                      function(x) eq_ricker(tmp_alpha[x], 
                                            tmp_beta[x], 
                                            U = 0)$Smsy_sum)
  
  # Get equilibrium initial recruitment
  R[1:nage,] <- rep(N0, each = nage)
  
  # Get equilibrium run
  for(s in 1:ns){
    Run[1:nage,,s] <- rep(rev(R[1:(nage),s] * rev(pReturn)), each = nage)
  }
  
  
  # Initial escapement-at-age and total escapement (0.25 initial U)
  UTmp <- rtnorm(nage, initUMean, initUSD, lower = 0, upper = 1)
  UtmpArr <- array(data = UTmp, dim = c(nage, nage, ns))
  S[1:nage,,] <- Run[1:nage,,] * (1 - UtmpArr)
  Stot[1:nage,] <- apply(S[1:nage,,], c(1,3), sum)
  
  # Initial catch-at-age and total catch
  C[1:nage,,] <- Run[1:nage,,] * UtmpArr
  Ctot[1:nage,] <- apply(C[1:nage,,], c(1,3), sum)
  
  # Initial observed run size (no observation error...)
  run_oe[1:nage,,] <-  Run[1:nage,,]
  
  # Initial observed recruitment
  for(s in 1:ns){
    R_oe[1:nage,s] <-  sum(diag(run_oe[(1:(nage)),,s]))
  }
  
  # Initial observed total stock size (no observation error...)
  Stot_oe[1:nage,] <- Stot[1:nage,]
  
  
  
  # Initially turn on flag in order to update escapement goal
  updateEGFlag <- TRUE
  
  # Loop over years
  for(y in (nage+1):ny2){
    
    # Get the run size so a run estimate can be calculated
    for(s in 1:ns){
      
      # Calculate the run size ... check math
      # Run[y,,s] <- get_raa(R = R[,s], yIdx = y, pRet = pReturn)
      Run[y,,s] <- rev(R[(y-nage):(y-1),s] * rev(pReturn))
      
    }

    # Pre-season run estimate
    runEst[y] <- rlnorm(1, mean = log(sum(apply(Run[y,,], 1, sum))) - 
                          oe_runEst^2/2, 
                        sd = oe_runEst)
    
    
    # Harvest rate for year y
    if(y < (nySpin + nage)){ # If there is not yet an EG, make random
      U[y] <- rtnorm(1, mean = initUMean, sd = initUSD, lower = 0, upper = 1)
      ## -- change the harvest intensity for the initial model -- ##
    }else{
      if(runEst[y] >= EG[y]){                   # if est run is larger than EG
        U[y] <- (runEst[y] - EG[y]) / runEst[y]
      }else{                                    # if est run is smaller than EG
        U[y] <- 0
      }
    }

    # Harvest rate including implementation error
    UImp[y] <- rlnormTrunc(1, meanlog = log(U[y]) - oe_U^2/2, sdlog = oe_U, 
                           min = 0, max = 1)
    
    # Loop over stocks
    for(s in 1:ns){
      
      # Escapement-at-age and total escapement
      S[y,,s] <- Run[y,,s] * (1 - UImp[y])
      Stot[y,s] <- sum(S[y,,s])
  
      # Catch-at-age and total catch
      C[y,,s] <- Run[y,,s] * UImp[y]
      Ctot[y,s] <- sum(C[y,,s])
      
      # Recruits spawned in year y
      R[y,s] <- ricker(alpha = tmp_alpha[s], beta = tmp_beta[s], 
                       S = Stot[y,s])
      
      
      # Add observation errors
      
      # Escapement and catch totals observed with error
      Stot_oe[y,s] <- ifelse(sampDsn[opt$s2s[i],s] > 0,
                             rlnorm(1, meanlog = log(Stot[y,s]) - 
                                      sampDsn[opt$s2s[i],s]^2/2, 
                                    sdlog = sampDsn[opt$s2s[i],s]),
                             0)
      Ctot_oe[y,s] <- ifelse(sampDsn[opt$s2s[i],s] > 0,
                             rlnorm(1, meanlog = log(Ctot[y,s]) -
                                      sampDsn[opt$s2s[i],s]^2/2, 
                                    sdlog = sampDsn[opt$s2s[i],s]),
                             0)
      
      
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
    ## -- ensure that n years have passed before if updateEGFlag == FALSE -- ##
    if(y > nySRMod + nage & updateEGFlag){

      # Get the appropriate number of years used to fit the assessment model
      # (from the end of the data set)
      s2sCol <- which(sampDsn[opt$s2s[i],] > 0)
      
      # Observed stock and recruits to feed into the model
      yrs2use <- (y-(nySRMod-1)):(y-nage)
      R_lm <- apply(R_oe[yrs2use, s2sCol, drop = FALSE], 1, sum)
      S_lm <- apply(Stot_oe[yrs2use, s2sCol, drop = FALSE], 1, sum)
      
      # Calculate the parameters of the Ricker model
      lnRS <- log(R_lm+1e-5) - log(S_lm+1e-5)
      SRlm <- try(lm(lnRS ~ S_lm))
      lRpar <- coef(SRlm)
      # abase <- lRpar[1]
      # bbase <- -lRpar[1] / lRpar[2]
      abase <- exp(lRpar[1])
      bbase <- -lRpar[2]
      
      # get unbiased estimates (H&W p. 269)
      sdR <- summary(SRlm)$sigma
      aprime <- abase + sdR^2/2
      bprime <- aprime / abase * bbase
      rMod[[y]] <- list(R_lm = R_lm, S_lm = S_lm,
                        aprime = aprime, bprime = bprime)

# calc Smsy ---------------------------------------------------------------
# test ----
# 
   ### Something not right ... Smsys should be closer... 
      # Calculate Smsy
      # Smsy[y,s] <- bprime * (0.5 - 0.07 * aprime)
      SmsyEst[y+1] <- log(aprime) / bprime * (0.5 - 0.07 * log(aprime))
      # SmsyEst[y+1] <- (1 - lambert_W0(exp(1 - aprime))) / bprime

      # cat('\n+++++++++++\n',
      #     log(aprime) / bprime * (0.5 - 0.07 * log(aprime)),
      #     '\n', (1-lambert_W0(exp(1 - aprime))) / bprime)

      # hmmmmmmm occasional negative estimates of Smsy.      
      if(SmsyEst[y+1] < 0){
        SmsyEst[y+1] <- SmsyEst[y]
        cat('... negative estimate of Smsy -- Smsy[i] <- Smsy[i-1] ...\n')
      }
      ## Determine the expansion factor for [stocks sampled]:[stocks in basin]
      # Stock sample ratio
      ssr <- sum(Smsy_true[s2sCol]) / sum(Smsy_true)
      basinExp <- 1/ssr
      
      # Calculate the CV of expanded productivity estimate
      prodCV <- get_prodCV(K = 8.5, b = 1000, p = ssr)
      
      SmsyScaled[y+1] <- rlnorm(n = 1,
                                meanlog = log(SmsyEst[y+1] * basinExp) - 
                                  prodCV^2/2,
                                sdlog = prodCV)

      updateEGFlag <- ifelse(updateEG, TRUE, FALSE)
      
      
      # Hi Mike.
      # Looks like that is exactly the type of relationship we are looking for!
      #   But are you accidently using alpha instead of ln(alpha) in the H&W 
      #   approximation?
      #   For stock #9 I get Smsy = 3711 using this approximation:
      # (log(alpha[9])*(0.5-0.07*log(alpha[9])))/beta[9]
      # And Smsy = 3718 using the Scheuerell (2016) exact solution:
      #   library("gsl")
      # (1−lambert_W0(exp(1 - alpha[9]))) / beta[9]
      # bc
      
      
      
    }else{
      
      SmsyEst[y+1] <- SmsyEst[y]
      SmsyScaled[y+1] <- SmsyScaled[y]
  
    }
    
    EG[y+1] <- SmsyScaled[y] * opt$egs[i]
    
    
    # Calculate overfished / extirpated status
    seqOut <- sapply(1:nrow(tmp_stpar),
                     function(x) SC.eq(U = UImp[y],
                                       a = tmp_alpha[x],
                                       b = tmp_beta[x]))
    OF[y,] <- seqOut['OF',]
    EX[y,] <- seqOut['EX',]
    
    
  } # close y
  
  ## Save performance metrics
  
  yrs2save <- (ny2-9):ny2
  
  # Average run size
  run2save <- apply(Run[yrs2save,,], 1, sum)
  meanRun[i] <- mean(run2save)
  
  # Average spawning stock size
  S2save <- apply(S[yrs2save,,], 1, sum)
  meanS[i] <- mean(S2save)
  
  # Average harvest
  h2save <- apply(Ctot[yrs2save,], 1, sum)
  meanH[i] <- mean(h2save)
  
  # Average estiamted Smsy
  Smsy2save <- SmsyScaled[yrs2save]
  meanSmsy[i] <- mean(Smsy2save, trim = 0.1)
  
  # Bias in Smsy
  basinSmsy <- eq_ricker(tmp_alpha, tmp_beta, U = 0)$Smsy_sum
  SmsyBias2save <- (SmsyEst[yrs2save] * basinExp - basinSmsy) / basinSmsy
  meanSmsyBias[i] <- mean(SmsyBias2save, trim = 0.1)
  
  # Overall percent overfished
  OF2save <- OF[yrs2save,]
  pctOF[i] <- sum(OF2save) / (nrow(OF2save) * ncol(OF2save))
  
  # Overall percent extirpated
  EX2save <- EX[yrs2save,]
  pctEX[i] <- sum(EX2save) / (nrow(EX2save) * ncol(EX2save))
  
  
  
  
  # For recording individual SR models...
  rModRecord <- rMod
  
  marks <- floor(seq(1, nrow(opt), length.out = 10))
  if(i %in% marks) cat(round(i / nrow(opt) * 100), '%', '\n', sep = '')
  
} # close i (options loop)


# Compile results
res <- cbind(opt, sampDsn[opt$s2s,], meanRun, meanH, meanSmsyBias, pctOF, pctEX) %>%
  as.data.frame() %>%
  as_tibble() %>%
  rowwise() %>%
  mutate(nweir = sum(across(starts_with('stock')) == cvAW[2]),
         nstockSamp = sum(across(starts_with('stock')) > 0),
         propWeir = nweir / nstockSamp) %>%
  ungroup() %>%
  mutate(nweirTxt = paste('nweir:', nweir),
         nstockSampTxt = paste('nstockSamp:', nstockSamp),
         nweirTxt = fct_reorder(nweirTxt, nweir),
         nstockSampTxt = fct_reorder(nstockSampTxt, nstockSamp),
         meanE = meanRun - meanH)


# Create new folder to store the results
dir.create('results', showWarnings = FALSE)
now <- format(Sys.time(), '%Y-%m-%d_%H-%M-%S')
figPath <- file.path('results', now)
dir.create(figPath, showWarnings = FALSE)

# Save the plots
get_plots(res = res, pth = figPath)

# compile summary document
rmdFigPath <- file.path('..', figPath)
rmarkdown::render('tpl/runSummaryTemplate.Rmd')

# Copy to appropriate results directory
file.copy(from = 'tpl/runSummaryTemplate.html',
          to = file.path(figPath, 'runSummary.html'),
          overwrite = FALSE)

