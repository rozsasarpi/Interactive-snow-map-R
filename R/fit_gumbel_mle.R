# Maximum likelihood fit using Gumbel distribution
#
#

fit_gumbel_mle = function(data) {
  
#   library(FAdist)
#   library(bbmle)
  
  sd_max   = sd(data)
  var_max  = sd_max^2
  mean_max = mean(data)
  
  scale    = sqrt(6)/pi*sd_max
  location = mean_max - scale*-digamma(1)
  
  nLL_gumbel = function(scale, location) {
    -sum(log(dgumbel(data, scale, location) ))
  }
  
  mle_fit = mle2(nLL_gumbel, start = list(scale = scale, location = location),
                   method="Nelder-Mead")

  return(mle_fit)
}