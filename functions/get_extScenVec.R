

# Function to extend vectors to be the length of the scenario

get_extScenVec <- function(v, outLen){
  
  if(length(v) == 1){
    out <- rep(v, outLen)
  }else{
    out <-v
  }

}


