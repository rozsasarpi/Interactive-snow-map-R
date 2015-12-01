plot_rprv = function(data, distr_type, input){
  
  # library(ismev)
  # distr_type = list(distr_type) # WARNING!
  
  # select the corresponding data and plot it
  data     = data[!is.na(data)]
  # remove years with zero snow
  data = data[!(data==0)]
  
  alpha_ci = input$alpha_ci
  nn       = 20
  RP       = logspace(1.01, 1900, 30)
  P        = 1- 1/RP
  
  nn       = length(P)
  
  k        = qnorm((1-alpha_ci)/2)
  empiF    = ((1:(length(data)))-2/5)/(length(data)+1/5)

  for (ii in 1:length(distr_type)){

    if (distr_type[ii] == "gev"){
      ## --------------------------------------------------------------------
      ## GEV
      ## --------------------------------------------------------------------
      # z = gev.fit(data, show = FALSE)
      # gev.diag(z)
      
      mle_fit = fit_gev_mle(data)
      V       = vcov(mle_fit)
      
      qq_d = matrix(nrow = nn, ncol = 3)
      for (jj in 1:nn) {
        # delta method
        g       = function(par) qgev(P[jj], par['shape'], par['scale'], par['location'])
        grad_g  =  jacobian(g, mle_fit@coef)
        
        sd_q    = sqrt(grad_g%*% V %*% t(grad_g))
        qq_d[jj,1]   = g(mle_fit@coef)
        qq_d[jj,2:3] = qq_d[jj,1] + k*c(-sd_q, sd_q)
      }
      
    } else if (distr_type[ii] == "gumbel") {
      ## --------------------------------------------------------------------
      ## Gumbel
      ## --------------------------------------------------------------------
      # z = gum.fit(data, show = FALSE)
      # gum.diag(z)
      
      mle_fit = fit_gumbel_mle(data)
      V       = vcov(mle_fit)
      
      qq_d = matrix(nrow = nn, ncol = 3)
      for (jj in 1:nn) {
        # delta method
        g       = function(par) qgumbel(P[jj], par['scale'], par['location'])
        grad_g  =  jacobian(g, mle_fit@coef)
        
        sd_q    = sqrt(grad_g%*% V %*% t(grad_g))
        qq_d[jj,1]   = g(mle_fit@coef)
        qq_d[jj,2:3] = qq_d[jj,1] + k*c(-sd_q, sd_q)
      }
    }
    
    df = data.frame(qq_d)
    names(df) = c("point", "ci_l", "ci_u")
    df$dist   = distr_type[ii]
    df$RP     = RP
    
    if (exists("df_mle_delta")) {
      df_mle_delta = rbind(df_mle_delta, df)
    } else {
      df_mle_delta = df
    }
    
  }
  
  
  #PLOT
  tRP = -log(-log(1-1/RP))
  
  plot(-log(-log(empiF)), sort(data), type = "p", ylab = expression('Annual maximum [kN/m'^2*']'), xlab = 'Return period [year]', xaxt = "n")
  lines(tRP, df_mle_delta[,1], col = "red")
  lines(tRP, df_mle_delta[,2])
  lines(tRP, df_mle_delta[,3])
  
  # set the axis
  xtick   = c(1.01, 2, 5, 10, 50, 100)
  t_xtick = -log(-log(1-1/xtick))
  axis(1, at = t_xtick, labels = xtick, las = 2)
  
  p = recordPlot()
  return(p)
}

