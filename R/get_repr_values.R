get_repr_values = function(data, distr_type){
  
  ## Exceptional snow check per EN:1991-1-3 background document (Sanpaolesi 1998)
  ## Characteristic value - 0.98 fractile based on the entire data
  
  # handle seasons with no snow
  P_snow    = sum(data > 0)/length(data)
  idx_snow  = !(data == 0)
  data      = data[idx_snow]
  
  # remove the maximum and fit model
  max_val   = max(data)
  max_idx   = which.max(data)
  data_     = data[-max_idx]
  
  P = 1-1/50
  # correction with snowfree seasons
  P = (P - (1 - P_snow))/P_snow
  
  
  if (distr_type == "gev"){
    ## --------------------------------------------------------------------
    ## GEV
    ## --------------------------------------------------------------------
    
    mle_fit  = fit_gev_mle(data)
    par      = mle_fit@coef
    q_char   = qgev(P, par['shape'], par['scale'], par['location'])
    
    mle_fit_ = fit_gev_mle(data_)
    par_     = mle_fit_@coef
    q_char_  = qgev(P, par_['shape'], par_['scale'], par_['location'])
    
    
  } else if (distr_type == "gumbel") {
    ## --------------------------------------------------------------------
    ## Gumbel
    ## --------------------------------------------------------------------
    
    mle_fit  = fit_gumbel_mle(data)
    par      = mle_fit@coef
    q_char   = qgumbel(P, par['scale'], par['location'])
    
    mle_fit_ = fit_gumbel_mle(data_)
    par_     = mle_fit_@coef
    q_char_  = qgumbel(P, par_['scale'], par_['location'])
    
  }
  
  k = max_val/q_char_
  
  R = list(char = q_char, mle_fit = mle_fit, exc_k = k)
  
  return(R)
}