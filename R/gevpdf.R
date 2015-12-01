gevpdf = function(x, shape, scale, location) {
  # parameters can be only scalars
  # x can be vector
  
  #   rm(list=ls(all=TRUE))
  #   x = c(10, 20, -9, 5)
  #   shape = 0
  #   scale = -1
  #   location = 1
  
  nx = length(x)
  nshape = length(shape)
  nscale = length(scale)
  nlocation = length(location)
  
  if (nshape*nscale*nlocation != 1) stop("The parameters can be scalars only!")
  
  
  
  if (any(is.na(c(shape, scale, location)))) {
    f = rep(NaN, nx)
  } else {
    
    f = vector(mode = "numeric", length = length(x))
    
    if (nx > 1){ 
      shape = rep(shape, nx)
      scale = rep(scale, nx)
      location = rep(location, nx)
    }
    
    
    # out of range of scale parameter
    idx_out = scale < 0
    f[idx_out] = NaN
    x = x[!idx_out]
    
    # Gumbel distribution
    idx_g =  abs(shape) < .Machine$double.eps
    if (any(idx_g)) {
      z = (x[idx_g]-location)/scale
      f[idx_g] = exp(-exp(-z) - z)
    }
    
    # out of the support, assign 0 to these values
    idx_0 = ((1 + shape*(x-location)/scale) <= 0) & !idx_g
    
    f[idx_0] = 0
    
    idx = !(idx_0 | idx_g)
    
    f[idx] = 1/scale[idx]*(1 + shape[idx]*(x[idx] - location[idx])/scale[idx])^(-1/shape[idx] - 1)*exp(-(1 + shape[idx]*(x[idx] - location[idx])/scale[idx])^(-1/shape[idx]))
    
  }
  
  return(f)
}