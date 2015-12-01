# MLE fit and confidence intervals for quantiles
#
# Confidence interval is estimated using:
# * delta method /df_mle_delta/
# * profile likelihood /df_mle_pl/
#
# NOTES:
# * fit_lnorm3_mle_.R might be problematic, double optization... _nlminb_
#

rm(list=ls(all=TRUE))

library(ggplot2)
library(scales)
library(XLConnect)
library(numDeriv)
library(FAdist)

source("fit_gumbel_mle_.R")
source("fit_gev_mle_.R")
source("fit_lnorm2_mle_.R")
source("fit_lnorm3_mle_.R")
source("fit_lnorm3_mom_.R")
source("fit_gev_mom_.R")
source("gevpdf.R")

working_dir = "F:/Working Folder/R Working Folder/hóteher/model_comp_&_stat_uncertainty_ESREL"
figure_dir = "f:/Working Folder/R Working Folder/hóteher/model_comp_&_stat_uncertainty_ESREL/figures/"

save_plot   = 1
write2excel = 1

setwd(working_dir)
## --------------------------------------------------------------------
## Control options
## --------------------------------------------------------------------
# Control options
pointID_v= c(300, 510, 941, 2735, 4553)
# pointID  = 510
pointID  = pointID_v[4]
alpha_ci = 0.9
RP       = c(50, 100, 300, 1000)
P        = 1- 1/RP

nn = length(P)
k  = -qnorm(0.5-alpha_ci/2)

load(file = paste(working_dir, "/data/swe_amax_", pointID, ".RData", sep = ""))

## --------------------------------------------------------------------
## LN2
## --------------------------------------------------------------------
mle_fit = fit_lnorm2_mle_(swe_amax)
V       = vcov(mle_fit)

qq_d = matrix(nrow = nn, ncol = 3)
for (i in 1:nn) {
  # delta method
  g       = function(par) qlnorm(P[i], par['meanlog'], par['sdlog'])
  grad_g  =  jacobian(g, mle_fit@coef)
  
  sd_q    = sqrt(grad_g%*% V %*% t(grad_g))
  qq_d[i,1]   = g(mle_fit@coef)
  qq_d[i,2:3] = qq_d[i,1] + k*c(-sd_q, sd_q) 
}

df_mle_delta  = data.frame(qq_d)
names(df_mle_delta) = c("point", "ci_l", "ci_u")
df_mle_delta$dist = "LN2"
df_mle_delta$RP   = RP

## --------------------------------------------------------------------
## LN3
## --------------------------------------------------------------------
mle_fit = fit_lnorm3_mle_(swe_amax)
V       = vcov(mle_fit)

qq_d = matrix(nrow = nn, ncol = 3)
for (i in 1:nn) {
  g       = function(par) qlnorm3(P[i], par['shape'], par['scale'], par['thres'])
  grad_g  =  jacobian(g, mle_fit@coef)
  
  sd_q    = sqrt(grad_g%*% V %*% t(grad_g))
  qq_d[i,1]   = g(mle_fit@coef)
  qq_d[i,2:3] = qq_d[i,1] + k*c(-sd_q, sd_q) 
}

df = data.frame(qq_d)
names(df) = c("point", "ci_l", "ci_u")
df$dist   = "LN3"
df$RP     = RP

df_mle_delta = rbind(df_mle_delta, df)

## --------------------------------------------------------------------
## Gumbel
## --------------------------------------------------------------------
mle_fit = fit_gumbel_mle_(swe_amax)
V       = vcov(mle_fit)

qq_d = matrix(nrow = nn, ncol = 3)
for (i in 1:nn) {
  # delta method
  g       = function(par) qgumbel(P[i], par['scale'], par['location'])
  grad_g  =  jacobian(g, mle_fit@coef)
  
  sd_q    = sqrt(grad_g%*% V %*% t(grad_g))
  qq_d[i,1]   = g(mle_fit@coef)
  qq_d[i,2:3] = qq_d[i,1] + k*c(-sd_q, sd_q)
}


df = data.frame(qq_d)
names(df) = c("point", "ci_l", "ci_u")
df$dist   = "Gumbel"
df$RP     = RP

df_mle_delta = rbind(df_mle_delta, df)

## --------------------------------------------------------------------
## GEV
## --------------------------------------------------------------------
mle_fit = fit_gev_mle_(swe_amax)
V       = vcov(mle_fit)

qq_d = matrix(nrow = nn, ncol = 3)
for (i in 1:nn) {
  # delta method
  g       = function(par) qgev(P[i], par['shape'], par['scale'], par['location'])
  grad_g  =  jacobian(g, mle_fit@coef)
  
  sd_q    = sqrt(grad_g%*% V %*% t(grad_g))
  qq_d[i,1]   = g(mle_fit@coef)
  qq_d[i,2:3] = qq_d[i,1] + k*c(-sd_q, sd_q)
}


df = data.frame(qq_d)
names(df) = c("point", "ci_l", "ci_u")
df$dist   = "GEV"
df$RP     = RP

df_mle_delta = rbind(df_mle_delta, df)

dff = df_mle_delta
## --------------------------------------------------------------------
## PLOT
## --------------------------------------------------------------------
x_ticks = c(50, 100, 300, 500, 750, 1000)

g = ggplot(dff, aes(x = RP, y = point, color = dist))
g = g + geom_path()
# g = g + geom_point(size = 4)
g = g + geom_pointrange(aes(ymin = ci_l, ymax = ci_u), position = "jitter", size = 1)
# g = g + geom_jitter
g = g + scale_x_continuous(breaks = x_ticks) +
  scale_y_continuous(breaks=pretty_breaks(n = 6))
g = g + theme_bw() + ylab("Return value, ML_point SWE [mm]") + xlab("Return period [year]") + 
  ggtitle(paste("ML point estimate with confidence intervals (", alpha_ci,"), delta method", sep = ""))
print(g)


if (save_plot == 1){
  ggsave(file = paste("ID",pointID, "_ML_delta_summary.eps", sep = ""), plot = g,
         path = figure_dir,
         width = 250, height = 125, units = 'mm')#, dpi = 1200)
}

## --------------------------------------------------------------------
## WRITE RESULTS TO EXCEL SHEET
## --------------------------------------------------------------------
if (write2excel == 1){
  # the sheet should be prepeared in advance!
  excel_file = "f:/Working Folder/Matlab Working Folder/snow load/reli_uncertainty_prop/model_comp_&_stat_uncertainty_ESREL/crude_tables.xlsx"
  sheet_name = paste("ID", pointID, sep = "")
  
  dist_names = c("LN2", "LN3", "Gumbel", "GEV")
  nn = length(dist_names)
  
  wb = loadWorkbook(excel_file)
  setStyleAction(wb, XLC$"STYLE_ACTION.NONE")
  
  # timestamp
  writeWorksheet(wb, date(), sheet_name, startRow = 6, startCol = 9, header = F) # WARNING, cell position
  
  # write point estimates and confidence interval endpoints per distributions
  for (i in 1:nn){
    mm = as.matrix(subset(dff, dist == dist_names[i])[,1:3])
    vv = matrix(t(mm), nrow = 1)
    
    writeWorksheet(wb, vv, sheet_name, startRow = 13+(i-1)*7, startCol = 4, header = F) # WARNING, cell position
  }
  
  saveWorkbook(wb)
}
