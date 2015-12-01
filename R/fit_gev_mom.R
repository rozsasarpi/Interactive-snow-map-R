#
#
#

fit_gev_mom = function(obs_max) {

#   library(FAdist)
#   library(e1071)
  
  # numerical problems -> sometimes produces NAs
  sd_max   = sd(obs_max)
  var_max  = sd_max^2
  mean_max = mean(obs_max)
  skew_max = skewness(obs_max, type = 2)
  
  #test
#   sd_max   = 10
#   var_max  = sd_max^2
#   mean_max = 14.28571
#   skew_max = 0.6
  
  
  ## GEV- type II, (Frechet) distribution
  # formulas of mean, variance and skewness is from [1998_Entropy-Based Parameter Estimation in Hydrology]
  # following the notation of the referenced book
  #
  # connection to wikipedia notation:
  # sigma = a (scale)
  # ksi = -b (shape)
  # mu = c (location)
  
  if(skew_max > 1.139547) {
     
    # numerical solution for parameter 'b'
  
    fun = function(b, skew_sample) {
      M_2_ = gamma(1 + 2*b) - gamma(1 + b)^2
      M_3_ = -gamma(1 + 3*b) + 3*gamma(1 + b)*gamma(1 + 2*b) - 2*gamma(1 + b)^3
      
      skew_ = -M_3_/M_2_^(3/2) # WARNING, - sign added, probably a typo in the book
      
      f = (skew_sample - skew_)^2
      
      return(f)
    }
    
    b = optimize(fun, c(-0.5, 0.5), skew_sample = skew_max)$minimum # a<b !!
    #fminbnd(fun, -0.5, 0, skew_sample = skew_max)
    
    a = sqrt(var_max*b^2/(gamma(1 + 2*b) - gamma(1 + b)^2))
    c = mean_max - a/b*(1 - gamma(1 + b))
    
    par = list(shape = -b, scale = a, location = c)
    
  }else {# Weibull distribution
    
    # following the wikipedia article notation (same as Coles.2001.Introduction the extreme value theory)
    # the notations should be unified..
    # numerical solution for parameter 'xi'
    
    fun = function(xi, skew_sample) {
      M_2_ = gamma(1 - 2*xi) - gamma(1 - xi)^2
      M_3_ = -gamma(1 - 3*xi) + 3*gamma(1 - xi)*gamma(1 - 2*xi) - 2*gamma(1 - xi)^3
      
      skew_ = M_3_/M_2_^(3/2) 
      
      f = (skew_sample - skew_)^2
      
      return(f)
    }
    
    xi = optimize(fun, c(-0.5, 0), skew_sample = skew_max)$minimum
    #fminbnd(fun, -0.5, 0, skew_sample = skew_max)
    
    sigma = sqrt(var_max*xi^2/(gamma(1 - 2*xi) - gamma(1 - xi)^2))
    mu = mean_max - sigma/xi*(gamma(1 - xi) - 1)
    
    par = list(shape = xi, scale = sigma, location = mu)
  }

  
  #if (any(is.na(q))){browser()}
  return(par)
}

