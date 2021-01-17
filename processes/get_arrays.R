

N <- array(data = NA, dim = c(ny2, nage, ns))
R <- matrix(data = NA, nrow = ny2, ncol = ns)
Run <- array(data = NA, dim = c(ny2, nage, ns))
S <- array(data = NA, dim = c(ny2, nage, ns))
Stot <- array(data = NA, dim = c(ny2, ns))
Stot_oe <- array(data = NA, dim = c(ny2, ns))
U <- numeric(ny2)
UImp <- numeric(ny2)
C <- array(data = NA, dim = c(ny2, nage, ns))
Ctot <- array(data = NA, dim = c(ny2, ns))
Ctot_oe <- array(data = NA, dim = c(ny2, ns))
paa_oe <- array(data = NA, dim = c(ny2, nage, ns))
run_oe <- array(data = NA, dim = c(ny2, nage, ns))
R_oe <- matrix(data = NA, nrow = ny2, ncol = ns)

OF <- matrix(data = NA, nrow = ny2, ncol = ns)
EX <- matrix(data = NA, nrow = ny2, ncol = ns)

# Smsy estimate and scaled estimate
SmsyEst <- numeric(ny2)
SmsyScaled <- numeric(ny2)

# Save results of model fitting
rMod <- list()

# Run size estimate
runEst <- numeric(ny2)

# Escapement goals
EG <- numeric(ny2)


