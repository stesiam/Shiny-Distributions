## Bernoulli functions

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

## Bernoulli return multiple values

bern <- function(prob){
  mean = prob
  var = prob * (1-prob)
  median = bern_median(prob)
  mode = median
  skewness = ((1-prob) - prob)/sqrt(prob*(1-prob))
  kurtosis = (1-6*prob*(1-prob))/(prob * (1- prob))
  entropy = -(1-prob)*log(1-prob) - prob*log(prob)
  
  bern_list = list("mean" = mean, "var" = var, "median" = median, "mode" = mode, "skewness" = skewness,
                   "kurtosis" = kurtosis, "entropy" = entropy)
  return(bern_list)
}


## Binomial 

binom <- function(n,p){
  mean = n*p
  var = n*p*(1-p)
  median = ceiling(n*p)
  mode = ceiling((n+1)*p)
  skewness = (1-p -p)/sqrt(n*p*(1-p))
  kurtosis = (1-6*p*(1-p))/(n*p*(1-p))
  
  binom_list = list("mean" = mean, "var" = var, "median" = median, "mode" = mode, "skewness" = skewness,
                                "kurtosis" = kurtosis)
  return(binom_list)
}

