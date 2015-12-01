prepare_data = function() {

# rm(list=ls(all=TRUE))
# 
# library(R.matlab)
# 
# # database code
# filepath = 'D:/Working folder/Matlab working folder/snow load/obs_database/database_code_swe.mat'
# db_code = as.data.frame(readMat(filepath, fixNames = F)$database_code)
# 
# names(db_code) = c('lng', 'lat', 'country', 'elevation')
# 
# # annual maxima data
# # path = 'D:/Working folder/Matlab working folder/snow load/time_trends_mle_ENGMECH/data'
# # filepath = file.path(path, 'annual_max_swe.mat')
# filepath = "D:/Working folder/Matlab working folder/snow load/obs_database/max_a_data.mat"
# 
# a_max = readMat(filepath, fixNames = F)$max_a
# 
# # replace missing values (-9999) with NAs
# a_max[a_max == -1] = NA
# 
# data = t(a_max)
# 
# year = vector("character", dim(data)[2])
# for (ii in 1:dim(data)[2]){
#   s = 1960+ii
#   e = 61+ii
#   if (e >= 100) e = e - 100
#   
#   year[ii] = paste(s, "/", e, sep = "")
# }
# 
# ##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# ##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# data = data/1000*9.81
# ##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# ##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# 
# A_MAX = list(value = data, year = year, code = db_code)
# save(A_MAX, file = 'data/snow_max_data.RData')
  
  ## Preapare raster images - do it once


  # already in kN/m2
  load('data/snow_max_data.RData')
  return(A_MAX)
  
}