
prepare_raster <- function(data2map, A_MAX, name){
  
  s = SpatialPixelsDataFrame(A_MAX$code[,c('lng', 'lat')], data = data.frame(data2map))
  crs(s) <- sp::CRS("+proj=longlat +datum=WGS84")
#   crs(s) <- sp::CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 
#                  +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs")
  r = raster(s)
  
  # resample to counteract drift/shift
#   r1 = r
#   nrow(r1) = 200
#   ncol(r1) = 200
#   r = resample(r, r1, method = "ngb")
  # resample to counteract drift/shift
  
  saveRDS(r, file = paste("data/raster_map_",name,".rds", sep = ""))
  # writeRaster(r, filename = paste("data/raster_map_",name,".grd", sep = ""), overwrite=TRUE)
  
  return()
}

