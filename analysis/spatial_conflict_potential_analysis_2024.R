#####################################################
##                                                 ##
##   Pinniped Fisheries Interaction Meta-Analysis  ##
##                                                 ##
## Spatial Conflict potential - analyses and plots ##
##                                                 ##
##                JJ- June 2024                    ##
##                                                 ##
#####################################################
rm(list = ls())
options(width = 100)
memory.limit(size = 60000) ## Increase memory limit to allocate bigger vectors for the rasters

library(raster)
library(terra)
library(tidyverse)
library(patchwork)
library(MetBrewer)
library(sf)

##______________________________________________________________________________
#### 1. Loading data ####

## Conflict potential raster
conflict_potential <- rast(x = "data/potential_pinniped_conflict_allpin.tif")

## Convert conflict to df with terra
conflict_df <- as.data.frame(x = conflict_potential, xy = TRUE)

## Operational interaction + spatial data
load("data/pinnrev_sf.RData")
load("data/op_interaction.RData")
load("data/pinnrev_centroid_data.RData")

## Overall world map - I am not hosting data, but this publicly available from https://www.naturalearthdata.com/
world_map <- st_read("../../Figures/Natural_Earth_Land_data/ne_10m_land.shp")

## Colour
conflict_colour <- "#7301A8FF"

##______________________________________________________________________________
#### 2. Linking to conflict values to operational interaction data ####

## extract raster values for the study geometries
pinnrev_mnconf <- pinnrev_sf %>% 
  mutate(potential_conflict = raster::extract(conflict_potential,
                     pinnrev_sf, fun = mean, na.rm = TRUE)[,2]) %>% 
  dplyr::select(acc_no, potential_conflict) %>% 
  as_tibble()

## Adding to operational interaction data
op_interaction <- op_interaction %>% 
  left_join(x = ., y = pinnrev_mnconf, by = "acc_no")

##______________________________________________________________________________
#### 3. Conflict figure ####

pinniped_conflict_studies <- ggplot() +
  geom_tile(data = conflict_df, 
            aes(x = x, y = y, fill = potential_pinniped_conflict_allpin)) +
  geom_sf(data = pinnrev_centroids, colour = "black", 
          size = 2.5, aes(shape = "Quantitative"), alpha = 1) +
  geom_sf(data = pinnrev_centroids_survey, colour = "black", 
          size = 2, aes(shape = "Non-standardised"), alpha = 1) + 
  scale_fill_gradient2(low = "grey99", mid = "grey91", 
                       high = "#7301A8FF") +
  scale_shape_manual(values = c(2,1)) +
  guides(fill = guide_colorbar(barheight = 8, barwidth = 2)) +
  labs(fill = "Potential for\npinniped-fishery\ninteractions", shape = "Data type") +
  theme_void() +
  theme(legend.margin = margin(0,0.2,0,0, "cm"))

ggsave(pinniped_conflict_studies,
       filename = "output/figure_4.pdf",
       width = 25, height = 13, units = "cm")

##______________________________________________________________________________
#### 4. Observed conflict vs. potential ####

## Potential for interactions density + observations
dat_dens1 <- ggplot(conflict_df, aes(x = potential_pinniped_conflict_allpin)) +
  geom_histogram(bins = 20, fill = "black") +
  geom_vline(data = op_interaction, 
                 aes(xintercept = potential_conflict), 
                 colour = conflict_colour, alpha = 0.8) +
  coord_cartesian(xlim = c(0,1)) +
  labs(x = NULL,
       y = "Frequency") +
  theme_bw(base_size = 13) +
  theme(panel.grid = element_blank())

dat_dens2 <- ggplot(op_interaction, aes(x = potential_conflict, y = "")) +
  geom_boxplot(fill = conflict_colour) +
  coord_cartesian(xlim = c(0,1)) +
  labs(x = "Potential for pinniped-fishery interactions",
       y = "") +
  theme_bw(base_size = 13) +
  theme(panel.grid = element_blank())

ggsave(dat_dens1 / dat_dens2 + plot_layout(heights = c(2,1)),
       filename = "output/figS5.jpeg",
       width = 18, height = 10, units = "cm", dpi = 1500)

##______________________________________________________________________________
#### 5. Other conflict potential indices ####

## Rasters and df
conflict_nogfw <- rast(x = "data/potential_pinniped_conflict_nogfw.tif")
conflict_noshore <- rast(x = "data/potential_pinniped_conflict_noshore.tif")

## Convert conflict to df with terra
nogfw_df <- as.data.frame(x = conflict_nogfw, xy = TRUE)
noshore_df <- as.data.frame(x = conflict_noshore, xy = TRUE)

# extract potential conflict of all layers
pinnrev_mnconf_full <- pinnrev_sf %>% 
  mutate(potential_conflict = raster::extract(conflict_potential, pinnrev_sf, fun = mean, na.rm = TRUE)[,2],
         potential_conflict_nogfw = raster::extract(conflict_nogfw, pinnrev_sf, fun = mean, na.rm = TRUE)[,2],
         potential_conflict_noshore = raster::extract(conflict_noshore, pinnrev_sf, fun = mean, na.rm = TRUE)[,2]) %>% 
  as_tibble() %>% 
  dplyr::select(acc_no, potential_conflict:potential_conflict_noshore) %>% 
  pivot_longer(- acc_no)

## plots
pinniped_conflict_full <- ggplot() +
  geom_tile(data = conflict_df, 
            aes(x = x, y = y, fill = potential_pinniped_conflict_allpin)) +
  scale_fill_gradient2(low = "grey99", mid = "grey91", 
                       high = "#7301A8FF") +
  guides(fill = guide_colorbar(barheight = 10, barwidth = 2)) +
  labs(fill = "Potential for\npinniped-fishery\ninteractions",
       tag = "a)") +
  theme_void(base_size = 13) 

pinniped_conflict_nogfw <- ggplot() +
  geom_tile(data = nogfw_df, 
            aes(x = x, y = y, fill = potential_pinniped_conflict_nogfw)) +
  scale_fill_gradient2(low = "grey99", mid = "grey91", 
                       high = "#194B5D") +
  guides(fill = guide_colorbar(barheight = 10, barwidth = 2)) +
  labs(fill = "Potential for\npinniped-fishery\ninteractions",
       tag = "b)") +
  theme_void(base_size = 13) 

pinniped_conflict_noshore <- ggplot() +
  geom_tile(data = noshore_df, 
            aes(x = x, y = y, fill = potential_pinniped_conflict_noshore)) +
  scale_fill_gradient2(low = "grey99", mid = "grey91", 
                       high = "#0D0887FF") +
  guides(fill = guide_colorbar(barheight = 10, barwidth = 2)) +
  labs(fill = "Potential for\npinniped-fishery\ninteractions",
       tag = "c)") +
  theme_void(base_size = 13) 


ggsave(pinniped_conflict_full / pinniped_conflict_nogfw / pinniped_conflict_noshore,
       filename = "output/figS6.jpeg",
       width = 25, height = 40, units = "cm", dpi = 800)


