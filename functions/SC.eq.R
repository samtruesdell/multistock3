#------------------------------------------------------------------------------#
# Status function
# estimates equilibrium spwn abundance and catch plus whether over fished 
# or extinct
# (Brendan C)
#------------------------------------------------------------------------------#

#U <- harvest rate
#a <- productivity
#b <- density dependence 

SC.eq <- function(U, a, b){
  
  a <- log(a)
  
  S.eq <- max(0, (a - (-log(1 - U))) / b)
  
  C.eq <- max(0,
              ((a-(-log(1 - U))) / b) * exp(a - b*((a - (-log(1 - U))) / b)) - 
                ((a - (-log(1 - U))) / b))
  
  OF <- ifelse(U > 0.5 * a - 0.07 * a^2, 1, 0)
  
  EX <- ifelse(S.eq == 0, 1, 0)
  
  return(c(S.eq = S.eq, C.eq = C.eq, OF = OF, EX = EX))
}
