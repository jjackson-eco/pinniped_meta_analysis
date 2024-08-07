#####################################################
##                                                 ##
##   Pinniped Fisheries Interaction Meta-Analysis  ##
##                                                 ##
##           Kroodsma et al. 2018 data             ##
##                                                 ##
##               JJ- May 26th 2022                 ##
##                                                 ##
#####################################################
rm(list = ls())
options(width = 100)
memory.limit(size = 60000) ## Increase memory limit to allocate bigger vectors for the rasters

## NOTE - I am not hosting data publicly, but available from https://globalfishingwatch.org/ 

library(raster)
library(rasterize)
library(tidyverse)
library(sf)
library(sp)

## Overall world data - I am not hosting data, but this publicly available from https://www.naturalearthdata.com/
world_map <- st_read("../../Figures/Natural_Earth_Land_data/ne_10m_land.shp")
world_raster <- raster(xmn=-180, xmx=180, ymn=-90, ymx=90, resolution=c(0.1,0.1))

##______________________________________________________________________________
#### 1. Test with 01/01/2012 ####

dat2012 <- read.csv("../../../Global_fishing_watch_data/mmsi-daily-csvs-10-v2-2012/2012-01-01.csv")

# use rasterize to create desired raster
r_data <- rasterize(x = dat2012[,3:2], # lon-lat data
                    y = world_raster, # raster object
                    field = dat2012[,6], # vals to fill raster with
                    fun = sum)

plot(r_data)

##______________________________________________________________________________
#### 2. Cycle through folders and extract data ####

# The 0.1 grid to project on to
world_raster <- raster(xmn=-180, xmx=180, ymn=-90, ymx=90, resolution=c(0.1,0.1))

# Year folders in the file system
GFW_years <- list.dirs("../../../Global_fishing_watch_data/")[2:10]

# Cycle through years
for(i in 1:length(GFW_years)){
  
  # current data files
  cdir = GFW_years[i]
  cfiles = list.files(cdir)
  
  # extract and bind all data for each year
  cdat = bind_rows(lapply(cfiles, function(x){
    dat = read_csv(paste0(cdir,"/",x), col_types = cols(), progress = F)
    cat("\r", round(which(cfiles == x)/length(cfiles), 3)*100, "% days complete         ")
    return(dat)
    }))
  
  # assign the data to the correct name
  dat_name = paste0("GFW_", substr(cdir, 
                                   start = nchar(cdir) - 3, 
                                   stop = nchar(cdir)))
  assign(dat_name, cdat)
  
  print(cdir)
  
}

# Bind all data
GFW_all <- bind_rows(GFW_2012, GFW_2013, GFW_2014, GFW_2015, GFW_2016,
                     GFW_2017, GFW_2018, GFW_2019, GFW_2020)

##______________________________________________________________________________
#### 3. Creating raster data ####

# use rasterize to create desired raster
GFW_raster <- rasterize(x = GFW_all[,3:2], # lon-lat data
                        y = world_raster, # raster object
                        field = GFW_all[,6], # vals to fill raster with
                        fun = sum)

plot(GFW_raster)
saveRDS(GFW_raster, file = "../../Data/GFW_raster.RData")

##______________________________________________________________________________
#### 4. Nice plot ####

# The CRS - Robin projection for curved edges
myCRS <- CRS("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
crs(GFW_raster) <- myCRS

GFW_raster_log10 <- log10(GFW_raster + 1)

GFW_raster_spdf <- as(GFW_raster, "SpatialPixelsDataFrame")
GFW_raster_df <- as.data.frame(GFW_raster_spdf)

plots_world <- ggplot() +
  geom_sf(data = world_map, fill = "lightgrey", colour = "lightgrey", size = 0.01) +
  geom_tile(data = GFW_raster_df, aes(x = x, y = y, fill = layer + 1)) +
  scale_fill_viridis_c(trans = "log10") +
  guides(fill = guide_colorbar(barheight = 10, barwidth = 2)) +
  labs(fill = "Fishing hours\nper 0.1 \U00B0") +
  theme_void()

ggsave(plots_world, filename = "../../Figures/extraction_20220512/Global_fishing_watch.jpeg", 
       width = 20, height = 14, units = "cm", dpi = 1000)

##______________________________________________________________________________
#### 5. Smaller raster ####

GFW_raster <- readRDS("../../Data/GFW_raster.RData")
res(GFW_raster)

GFW_raster_0.5 <- aggregate(GFW_raster, fact = 5)
saveRDS(GFW_raster_0.5, file = "../../Data/GFW_raster_0.5.RData")

