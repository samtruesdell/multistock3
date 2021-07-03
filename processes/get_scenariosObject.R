


# Does not run all combinations of scenario parameters.
# Scenario parameter lengths must be *either* 1 or the same length
# as other parameters that are != length 1

# Define the max length
spar <- list(nrep = nrep, nySRMod = nySRMod, 
             ns_low = ns_low, ns_med = ns_med, ns_high = ns_high, 
             ny = ny, nySpin = nySpin, initUMean = initUMean, initUSD = initUSD, 
             cvA = cvA, cvW = cvW, 
             oe_runEst = oe_runEst, oe_paaS = oe_paaS, oe_U = oe_U)

sparLen <- sapply(spar, length)

# Ensure that all values are either length == 1 or one other value
if(length(unique(sparLen)) > 2){
  stop('Ensure all parameter lengths are either exactly 1 or equal to ',
             '1 or one other value only.\nLengths:\n',
             paste0(names(sparLen), ': ', sparLen, '\n'))
}


scenLst <- lapply(spar, get_extScenVec, outLen = max(sparLen))

# add the list elements to the global environment
list2env(scenLst, globalenv())




