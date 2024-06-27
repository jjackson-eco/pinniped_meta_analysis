#####################################################
##                                                 ##
##   Pinniped Fisheries Interaction Meta-Analysis  ##
##                                                 ##
##            Create Pinniped Polygons             ##
##                                                 ##
##               JJ- Jul 25th 2022                 ##
##                                                 ##
#####################################################
rm(list = ls())
options(width = 100)
memory.limit(size = 60000) ## Increase memory limit to allocate bigger vectors for the rasters

## NOTE - I am not hosting data publicly, but available from IUCN red list, and on request

library(raster)
library(rasterize)
library(tidyverse)
library(sf)
library(sp)

##______________________________________________________________________________
#### 1. Loading data ####

r_list <- sf::st_read("../../Data/MAMMALS_MARINE_AND_TERRESTRIAL/MAMMALS_MARINE_AND_TERRESTRIAL.shp")
r_list1 <- sf::st_read("../../Data/MAMMALS_FRESHWATER/MAMMALS_FRESHWATER.shp")
r_list3 <- sf::st_read("../../Data/MAMMALS_MARINE_ONLY/MAMMALS_MARINE_ONLY.shp")
P_groenlandicus <- sf::st_read("../../Data/Pagophilus_groenlandicus/data_0.shp")
H_fasciata <- sf::st_read("../../Data/Histriophoca_fasciata/data_0.shp")

##______________________________________________________________________________
#### 2. Wrangle to only include Pinnipeds ####

pinniped_sf_dat <- rbind(r_list, r_list1, r_list3) %>% 
  filter(family %in% c("OTARIIDAE", "ODOBENIDAE", "PHOCIDAE") == TRUE)

# ggplot(data = pinniped_sf_dat, aes(fill = binomial)) +
#   geom_sf()

## Only non-threatened species
pinniped_lc_sf <- filter(pinniped_sf_dat,
                         category == "LC")

##______________________________________________________________________________
#### 3. Save ####

save(pinniped_sf_dat, pinniped_lc_sf,
     file = "../../Data/pinniped_sf_dat.RData")










