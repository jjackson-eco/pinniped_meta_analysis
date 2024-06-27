#####################################################
##                                                 ##
##   Pinniped Fisheries Interaction Meta-Analysis  ##
##                                                 ##
##              Raster aggregation                 ##
##                                                 ##
##               JJ- Jul 25th 2022                 ##
##                                                 ##
#####################################################
rm(list = ls())
options(width = 100)
memory.limit(size = 60000) ## Increase memory limit to allocate bigger vectors for the rasters

## NOTE - I am not hosting data publicly, but available from https://oceancolor.gsfc.nasa.gov/resources/docs/distfromcoast/

library(raster)
library(rasterize)
library(tidyverse)
library(sf)

##______________________________________________________________________________
#### 1. Loading + Wrangling ####

coast_dist_raw <- raster("../../../Coastal_data/GMT_intermediate_coast_distance_01d.tif")

summary(coast_dist_raw)

coast_dist_0.5 <- aggregate(coast_dist_raw, fact = 50)
res(coast_dist_0.5)

writeRaster(x = coast_dist_0.5, filename = "../../Data/coast_dist_0.5.tif")







