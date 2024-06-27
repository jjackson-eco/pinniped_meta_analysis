#####################################################
##                                                 ##
##   Pinniped Fisheries Interaction Meta-Analysis  ##
##                                                 ##
##       Preliminary Binomial Meta-analyses        ##
##                                                 ##
##             JJ- March 31st 2022                 ##
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
library(meta)
library(metafor)
library(MetBrewer)
library(gghalves)

##______________________________________________________________________________
#### 1. Loading and wrangling ####

## Full data
pinnrev <- read_csv("../../Data/scoping_data_extraction_20220331.csv") %>% 
  mutate(spp = paste0(genus, " ", species))

glimpse(pinnrev)

## Operational interaction data - non survey results with normalising constants
op_interaction <- pinnrev %>% 
  filter(method != "survey" &
           is.na(norm_int) == FALSE) %>% 
  mutate(interaction_prop = x_int,
         interaction_trials = n_int*norm_int,
         damage_prop = as.numeric(x_dmg),
         damage_trials = as.numeric(n_dmg)*norm_int,
         location = case_when(
           fish_loc == "off" ~ "off_shore",
           fish_loc == "near" ~ "near_shore",
           TRUE ~ "both")) %>% 
  # Adding in events
  mutate(interaction_events = floor(interaction_prop*interaction_trials),
         damage_events = floor(damage_prop*damage_trials)) %>% 
  dplyr::select(acc_no, score, obs_no, author_type = auth_type,
                country = country, prevention = prev, compensation = comp,
                retaliation = retal, species = spp, pop_trend, fishery_type = fish_type,
                gear_type = fish_gear, location, 
                interaction_prop, interaction_events, interaction_trials,
                damage_prop, damage_events, damage_trials)

##______________________________________________________________________________
#### 2. Meta-analyses preliminary ####

#__________________________________________________________
## 2a. filtering for complete cases for both dependent vars
int_dat <- filter(op_interaction, is.na(interaction_events) == FALSE)
dmg_dat <- filter(op_interaction, is.na(damage_events) == FALSE)

#__________________________________
## 2b. The meta analyses in metafor
int_mod_base <- metaprop(interaction_events, interaction_trials, 
                         studlab = acc_no, sm = "PLOGIT", data = int_dat, 
                         method="Inverse", method.tau="DL")

summary(int_mod_base)

dmg_mod_base <- metaprop(damage_events, damage_trials, 
                         studlab = acc_no, sm = "PLOGIT", data = dmg_dat, 
                         method="Inverse", method.tau="DL")

summary(dmg_mod_base)

#_________________________________________
## 2c. Forest plot to show model summaries
jpeg("../../Figures/extraction_20220331/meta_regression_interaction_summary.jpeg",
     width = 35, height = 20, units = "cm", res = 1000)
forest.meta(int_mod_base, layout="RevMan5", xlab="Proportion", 
            comb.r=T, comb.f=F, xlim = c(0,1), fontsize=10, digits=3)
dev.off()

jpeg("../../Figures/extraction_20220331/meta_regression_damage_summary.jpeg",
     width = 35, height = 20, units = "cm", res = 1000)
forest.meta(dmg_mod_base, layout="RevMan5", xlab="Proportion", 
            comb.r=T, comb.f=F, xlim = c(0,1), fontsize=10, digits=3)
dev.off()

##______________________________________________________________________________
#### 3. Overall distributions ####

int_plot <- op_interaction %>% 
  filter(is.na(interaction_prop) == FALSE) %>% 
  ggplot(aes(y = interaction_prop, x = as.factor(1), 
             size = interaction_trials)) +
  geom_violin(aes(size = NULL), adjust = 0.7, 
              fill = MetPalettes$Hokusai2[[1]][3], alpha = 0.1, 
              colour = "white",show.legend = FALSE) +
  geom_point(alpha = 0.5, colour = MetPalettes$Hokusai2[[1]][3]) +
  # Model predictions
  geom_errorbar(aes(ymax = 0.524, ymin = 0.232, size = NULL), 
                width = 0, size = 1.2, show.legend = FALSE) +
  geom_point(aes(y = 0.366, size = NULL), size = 5, show.legend = FALSE) +
  scale_size_continuous(breaks = c(0, 5, 50, 250, 1000, 1500), 
                        range = c(2,8), trans = "log10")  +
  labs(x = NULL, y = "Proportion of fishing days with pinniped interactions",
       size = "Number\nof days") +
  scale_y_continuous(limits = c(0,1)) +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_blank(),
        panel.grid = element_blank())

dmg_plot <- op_interaction %>% 
  filter(is.na(damage_prop) == FALSE) %>% 
  ggplot(aes(y = damage_prop, x = as.factor(1), 
             size = interaction_trials)) +
  geom_violin(aes(size = NULL), adjust = 0.7, 
              fill = MetPalettes$Hokusai2[[1]][6], alpha = 0.1, 
              colour = "white", trim = FALSE, show.legend = FALSE) +
  geom_point(alpha = 0.5, colour = MetPalettes$Hokusai2[[1]][6]) +
  # Model predictions
  geom_errorbar(aes(ymax = 0.193, ymin = 0.073, size = NULL), 
                width = 0, size = 1.2, show.legend = FALSE) +
  geom_point(aes(y = 0.121, size = NULL), size = 5, show.legend = FALSE) +
  scale_size_continuous(breaks = c(0, 5, 50, 250, 1000, 1500), 
                        range = c(2,8), trans = "log10")  +
  labs(x = NULL, y = "Proportion of catch lost",
       size = "Number\nof days") +
  scale_y_continuous(limits = c(0,1)) +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_blank(),
        panel.grid = element_blank())

ggsave(int_plot + dmg_plot, 
       filename = "../../Figures/extraction_20220331/Interaction_Damage_Distribution.jpeg",
       width = 26, height = 13, units = "cm", dpi = 1000)

##______________________________________________________________________________
#### 4. Meta-regression ####

#_______________________________________________
## 4a. Interactions by fishery size and pop trend
int_fishery_type <- metareg(int_mod_base, ~ fishery_type)
int_pop_trend <- metareg(int_mod_base, ~ pop_trend)

#_______________________________________________
## 4b. Damage by fishery size and pop trend
dmg_fishery_type <- metareg(dmg_mod_base, ~ fishery_type)
dmg_pop_trend <- metareg(dmg_mod_base, ~ pop_trend)









