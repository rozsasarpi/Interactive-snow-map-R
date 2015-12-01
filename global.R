library(shiny)
library(leaflet)
library(dplyr)
library(sp)
library(raster)
library(FAdist)
library(e1071)
library(bbmle)
library(ggplot2)
library(numDeriv)


source("R/fit_gev_mom.R", local = TRUE)
source("R/fit_gev_mle.R", local = TRUE)
source("R/fit_gumbel_mle.R", local = TRUE)

source("R/gevpdf.R", local = TRUE)
source("R/get_repr_values.R", local = TRUE)

source("R/prepare_data.R", local = TRUE)
source("R/prepare_raster.R", local = TRUE)
source("R/plot_point_hist.R", local = TRUE)
source("R/plot_rprv.R", local = TRUE)

# Global
A_MAX = prepare_data()

## --------------------------------------------------------------------
# Prepare raster images to plot - run only if input data have changed
# map_mean = rowMeans(A_MAX$value, na.rm = T)
# map_cov  = apply(A_MAX$value, 1, function(x) sd(x, na.rm = T)/mean(x, na.rm = T))
# map_skew = apply(A_MAX$value, 1, function(x) skewness(x, na.rm = T, type = 2))
# 
# prepare_raster(map_mean, A_MAX, "mean")
# prepare_raster(map_cov, A_MAX, "cov")
# prepare_raster(map_skew, A_MAX, "skew")
## --------------------------------------------------------------------
r_mean  = readRDS(paste("data/raster_map_mean.rds", sep = ""))
r_cov   = readRDS(paste("data/raster_map_cov.rds", sep = ""))
r_skew  = readRDS(paste("data/raster_map_skew.rds", sep = ""))

# Load meteorological stations
load("data/carpatclim_stations.RData")

stations$StationType = as.factor(stations$StationType)
# Remove the stations with NA coordinates or stationtypes
idx = !is.na(stations$Actual.Longitude) & !is.na(stations$Actual.Latitude) & !is.na(stations$StationType)
stations = stations[idx,]


# Get the bounding frame
lng = unique(A_MAX$code$lng)
lat_llim = sapply(lng, function(x) min(A_MAX$code$lat[A_MAX$code$lng == x]))
lat_ulim = sapply(lng, function(x) max(A_MAX$code$lat[A_MAX$code$lng == x]))
bound_lat = c(rev(lat_ulim), lat_llim)
bound_lng = c(rev(lng), lng)

# Icons for meteorological stations
station_icons <- iconList(
  A = makeIcon(iconUrl = "include/automatic.png", iconWidth = 10, iconHeight = 10),  #automatic
  M = makeIcon(iconUrl = "include/manual.png", iconWidth = 10, iconHeight = 10),     #manual
  X = makeIcon(iconUrl = "include/combination.png", iconWidth = 10, iconHeight = 10) #combined
)

## --------------------------------------------------------------------
## FUNCTIONS
## --------------------------------------------------------------------
logspace <- function(a, b, n) {
  exp(log(10)*seq(log10(a), log10(b), length.out = n))
}