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

## Poisson

poisson <- function(lambda){
  mean = lambda
  var = lambda
  median = floor(lambda + 1/3 - (0.02)/lambda )
  mode  = floor(lambda)
  skewness = lambda^(-0.5)
  kurtosis = 1/lambda
  
  poisson_list = list("mean" = mean, "var" = var, "median" = median, "mode" = mode, "skewness" = skewness,
                      "kurtosis" = kurtosis)
  return(poisson_list)
}


## Geometric0

geometric0<- function(p){
  q = 1-p
  mean = q/p
  var = q/p^2
  median = ceiling( -1/(log2(q))) -1
  mode  = 0
  skewness = (2-p)/sqrt(q)
  kurtosis = 6 + p^2/q
  
  geom0_list = list("mean" = mean, "var" = var, "median" = median, "mode" = mode, "skewness" = skewness,
                      "kurtosis" = kurtosis)
  return(geom0_list)
}

## Geometric1

geometric1<- function(p){
  q = 1-p
  mean = 1/p
  var = q/p^2
  median = ceiling( -1/(log2(q))) -1
  mode  = 0
  skewness = (2-p)/sqrt(q)
  kurtosis = 6 + p^2/q
  
  geom1_list = list("mean" = mean, "var" = var, "median" = median, "mode" = mode, "skewness" = skewness,
                    "kurtosis" = kurtosis)
  return(geom1_list)
}

## Negative Binomial 0

mode_nb0 <- function(r,p){
  if (r <= 1){
    mode = 0
  }
  else{
    mode = floor(p*(r-1)/(1-p))
  }
  return(mode)
}

nb0<- function(r,p){
  q = 1-p
  mean = r*q/p
  var = r*q/p^2
  median = "..."
  mode  = mode_nb0(r,p)
  skewness = (1 + p)/sqrt(p*r)
  kurtosis = 6/r + q^2/p*r
  
  nb0_list = list("mean" = mean, "var" = var, "median" = median, "mode" = mode, "skewness" = skewness,
                    "kurtosis" = kurtosis)
  return(nb0_list)
}

## TODO: Negative Binomial 1


## Hypergeometric 

hypergeom <- function(n,a,b){
  mean = a*b/n
  var = mean* ((n-a)/n) * ((n-b)/(n-1))
  median = "..."
  mode = floor((a+1)*(b+1)/(n+2))
  skewness = "..."
  kurtosis = "..."
  }

## Discrete Uniform

duniform <- function(a,b){
  n = b -a + 1
  mean = (a+b)/2
  var = (n^2 - 1)/12
  median = mean
  mode = NA
  skewness = 0
  kurtosis = -(6*n^2+1)/(5*n^2 -1)
  
  dunif_list = list("mean" = mean, "var" = var, "median" = median, "mode" = mode, "skewness" = skewness,
                  "kurtosis" = kurtosis)
  return(dunif_list)
}


### Continuous Distributions

## Normal distribution

normaldist <- function(mean_normal, var){
  mean = mean_normal
  var = var
  median = mean_normal
  mode = mean_normal
  skewness = 0
  kurtosis = 0
  entropy = 0.5*log10(2 * pi * var) + 0.5
    
normaldist_list = list("mean" = mean, "var" = var, "median" = median, "mode" = mode, "skewness" = skewness,
                      "kurtosis" = kurtosis, "entropy" = entropy)
  
return(normaldist_list)
}

## Uniform Distribution


unifdist <- function(a,b){
  mean = (a+b)/2
  var = ((b-a)^2)/12
  median = (a+b)/2
  mode = "..."
  skewness = 0
  kurtosis = -6/5
  entropy = log(b-a)
  
  unifdist_list = list("mean" = mean, "var" = var, "median" = median, "mode" = mode, "skewness" = skewness,
                         "kurtosis" = kurtosis, "entropy" = entropy)
  
  return(unifdist_list)
}



## Exponential Distribution

expodist <- function(lambda){
  mean = 1/lambda
  var = 1/lambda^2
  median = log(2)/lambda
  mode = 0
  skewness = 2
  kurtosis = 6
  entropy = 1 - log(lambda)
  
  exp_list = list("mean" = mean, "var" = var, "median" = median, "mode" = mode, "skewness" = skewness,
                       "kurtosis" = kurtosis, "entropy" = entropy)
  
  return(exp_list)
}

## TODO : Beta 





## TODO : Weibull




