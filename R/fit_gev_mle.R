# Maximum likelihood fit using GEV distribution
#
#

fit_gev_mle = function(data) {

#   library(FAdist)
#   library(bbmle)
#   source("fit_gev_mom_.R")
#   source("gevpdf.R")

  nLL_gev <- function(shape, scale, location) {
    -sum(log(gevpdf(data, shape = shape, scale = scale, location = location) ))
    # dgev function from FAdist package gives wrong values outside of the support!
  }
  
  par_mm  = fit_gev_mom(data)
  
  # to avoid Infinite loglikelihood at initial point for Weibull types
  if (par_mm$shape < 0) {
    location_min = (max(data) + par_mm$scale/par_mm$shape)*1.1
    par_mm$location = max(par_mm$location, location_min)
  }
  
  # to avoid Infinite loglikelihood at initial point for Frechet types
  if (par_mm$shape > 0) {
    location_max = (min(data) + par_mm$scale/par_mm$shape)*0.9
    par_mm$location = min(par_mm$location, location_max)
  }
  
  mle_fit = mle2(minuslogl = nLL_gev, start = par_mm, optimizer = "nlminb", 
                 lower = list(shape = -0.5, scale = 1e-2, location = -Inf), upper = list(shape = 0.5, scale = Inf, location = Inf))
#   mle_fit = mle(nLL_gev, start = list(shape = par_mm$shape, scale = par_mm$scale, location = par_mm$location),
#                 method="Nelder-Mead")
  
#   # depends on ismev package + yield different output than _mle_
#   mle_fit  = gev.fit(data, show = FALSE)

  return(mle_fit)
}

