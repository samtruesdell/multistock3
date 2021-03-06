


# number of repetitions
nrep <- 3

# number of years in the SR model
nySRMod <- 30


# number of stocks to be monitored
ns_low <- 4
ns_med <- 5
ns_high <- 4

# Levels for alphas and betas
abounds <- matrix(c(5, 8,
                    3, 5,
                    1.5,3),
                  nrow = 3, byrow = TRUE)

bbounds <- matrix(c(0.001, 0.003,
                    0.00055, 0.001,
                    0.0001, 0.00055),
                  nrow = 3, byrow = TRUE)

# number of management simulation years
ny <- 50

# Number of initial simulation years before model fitting
nySpin <- 25

# Number of years to use to fit the Ricker model used in management
# nyFit <- 30

# Initial U
initUMean <- 0.8
initUSD <- 0.1

# CVs for aerial surveys [1] and weirs [2] and ESS for weirs
# and correlations btw aerial surveys and weirs
cvAW <- c(1, 0.01)

# SD for run size estimate
oe_runEst <- 0.1


oe_paaS <- 50
# oe_catch <- 0.1
oe_U <- 0.1
# oe_S <- 0.1

# CV for the harvest of every stock
# cvH <- 0.02

# escapement goal scalars
egscalar <- seq(from = 0.01, to = 2, length.out = 30)

# # number of repetitions to average the results over for each combination
# # of mType and EG
# nrepProcess <- 30
# 
# # Number of repetitions to average the results over for re-arranging the
# # mType grid (i.e., sampling distribution of stocks)
# nrepGrid <- 100

# number of years to use to fit SR model
# sryrs <- 30

# Probability of return by age class
pReturn = c(a1=0, a2=0, a3=0, a4=.2, a5=.39, a6=.38, a7=.03)

# Whether to update the escapement goal every year or not
updateEG <- FALSE





