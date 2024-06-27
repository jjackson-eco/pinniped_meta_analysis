#####################################################
##                                                 ##
##   Pinniped Fisheries Interaction Meta-Analysis  ##
##                                                 ##
##            Survey data investigation            ##
##                                                 ##
##               JJ- July 27th 2022                ##
##                                                 ##
#####################################################
rm(list = ls())
options(width = 100)

## Change the .libPaths and R_LIBS_USER to the right thing if you're on a uni computer
if(Sys.info()["nodename"] == "BIO-W-LT-000083" |
   Sys.info()["nodename"] == "BIO-W-DT-02108") {
  .libPaths("C:/Users/zool2541/R-4.1.1/library/")
  .libPaths("!\\\\zoo-suitcase/home$/zool2541/My Documents/R/win-library/4.1")}

library(tidyverse)
library(patchwork)
library(flextable)
library(MetBrewer)
library(meta)
library(metafor)

##______________________________________________________________________________
#### 1. Loading and wrangling ####

load("../../Data/study_names.RData")

## Interaction data
pinnrev <- read_csv("../../Data/scoping_data_extraction_20220811.csv") %>% 
  mutate(spp = paste0(genus, " ", species)) %>% 
  left_join(x = ., y = study_names, by = "acc_no")

glimpse(pinnrev)

## Survey data
survey_data_raw <- pinnrev %>% 
  filter(method == "survey")

survey_data_raw[26:27,"x_dmg"] <- "0.25"

survey_data <- survey_data_raw %>% 
  mutate(x_dmg = as.numeric(x_dmg),
         x_dmg = if_else(x_dmg > 1, x_dmg/100, x_dmg)) %>% 
  group_by(acc_no) %>% 
  summarise(study_name = study[1],
            interaction_prop = mean(x_int), 
            damage_prop = mean(x_dmg))

## Fixing unicode study names
survey_data[c(7,8,12),2] <- c("Gonzalez and de Larrinoa 2013",
                              "Guclusoy 2008",
                              "Sepulveda and Oliva 2005")

survey_data$modality <- c("Proportion of fishers +\nSelf rerporting", 
                          "Self reporting",
                          "Proportion of fishers", "Self reporting",
                          "Proportion of fishers", "Proportion of fishers",
                          "Proportion of fishers", "Self reporting",
                          "Proportion of fishers", "Proportion of fishers",
                          "Proportion of fishers", "Proportion of fishers")

save(survey_data, file = "../../Data/survey_data_July2022.RData")

##______________________________________________________________________________
#### 2. Plot and summary statistics
int_colour <- MetPalettes$Hokusai2[[1]][3]
dmg_colour <- MetPalettes$Hokusai2[[1]][6]

interaction_plot <- ggplot(survey_data, aes(x = "", y = interaction_prop)) +
  geom_violin(fill = int_colour, alpha = 0.2, 
              colour = "white", show.legend = F) +
  geom_point(aes(x = "", shape = modality), colour = int_colour, 
             size = 6, alpha = 0.8, show.legend = F) +
  coord_cartesian(ylim = c(0,1)) +
  labs(x = "", y = "Interaction proportion", 
       shape = "Survey\nmodality", tag = "a)") +
  theme_bw(base_size = 13) +
  theme(panel.grid = element_blank())

damage_plot <- ggplot(survey_data, aes(x = "", y = damage_prop)) +
  geom_violin(fill = dmg_colour, alpha = 0.2, 
              colour = "white", show.legend = F) +
  geom_point(aes(x = "", shape = modality), colour = dmg_colour, 
             size = 6, alpha = 0.8) +
  coord_cartesian(ylim = c(0,1)) +
  labs(x = "", y = "Damage proportion", 
       shape = "Survey\nmodality", tag = "b)") +
  guides(shape = guide_legend(override.aes = 
                                list(colour = "black",
                                     alpha = 1))) +
  theme_bw(base_size = 13) +
  theme(panel.grid = element_blank())

summarise(survey_data, 
          mean_int = mean(interaction_prop, na.rm = T),
          mean_dmg =  mean(damage_prop, na.rm = T))

ggsave(interaction_plot + damage_plot, 
       filename = "../../Figures/extraction_20220512/survey_summary.jpeg",
       width = 22, height = 12, units = "cm", dpi = 1000)
