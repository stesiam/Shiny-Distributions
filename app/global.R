## Bernoulli functions

bern_mean <- function(prob) {
  result <- prob
  return(result)
}

bern_var <- function(prob) {
  result <- prob * (1-prob)
  return(result)
}

bern_median <- function(prob){
  if (prob < 0.5){
    result = 0
  }
  else if (prob > 0.5){
    result =1
  }
  else{
    result = "[0,1]"
  }
  return(result)
}

bern_mode <- function(prob){
  if (prob < 0.5){
    result = 0
  }
  else if (prob > 0.5){
    result =1
  }
  else{
    result = "[0,1]"
  }
  return(result)
}


bern_skewness <- function(prob){
  result = ((1-prob) - prob)/sqrt(prob*(1-prob))
  return(result)
}

bern_kurtosis <- function(prob){
  result = (1-6*prob*(1-prob))/(prob * (1- prob))
  return(result)
}

bern_entropy <- function(prob){
  result = -(1-prob)*log(1-prob) - prob*log(prob)
  return(result)
}

## Poisson functions


