#####################################################
##                                                 ##
##   Pinniped Fisheries Interaction Meta-Analysis  ##
##                                                 ##
##       Preliminary Binomial Meta-analysis        ##
##                                                 ##
##             JJ- January 13th 2021               ##
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

##______________________________________________________________________________
#### 1. Loading and wrangling ####

## Interaction data
pinnrev <- read_csv("../../Data/scoping_data_extraction_20211209.csv") %>% 
  mutate(spp = paste0(genus, " ", species))

glimpse(pinnrev)

## Effort categories
effort_categories <- 
  read_csv("../../Scoping_Survey/Effort_interaction_classifications.csv") %>% 
  pivot_longer(-Study, names_to = "effort_category") %>% 
  filter(is.na(value) == FALSE) %>% 
  mutate(effort_category = gsub(pattern = "_", 
                                replacement = "/", 
                                x = effort_category)) %>% 
  group_by(Study) %>% 
  summarise(effort_category = effort_category[n()]) %>% # Select only the most detailed one
  ungroup() 

effort_categories

## Date conversion data
effort_day <- read_csv("../../Data/effort_day_conversion_20211209.csv") %>% 
  dplyr::select(acc_no = Study, n_int_days) # just keep necessary info

# Restricting to only studies with number of interactions and pinniped interactions
propint_dat <- pinnrev %>% 
  left_join(x = ., y = effort_day, by = "acc_no") %>% 
  filter(is.na(x_int) == FALSE & 
           is.na(n_int_days) == FALSE & 
           n_int < 1000) %>% # Not sure what to do with the Scottish study with so many days
  mutate(pinn_interactions = n_int_days*x_int,
         total_interactions = n_int_days)

glimpse(propint_dat)

# adding in effort categories
propint_dat <- left_join(propint_dat, y = effort_categories,
                         by = c("acc_no" = "Study"))

##______________________________________________________________________________
#### 2. Exploratory plots ####

# Author type
authtype_int <- ggplot(propint_dat, aes(x = auth_type, y = x_int, 
                        size = n_int_days, colour = auth_type)) +
  geom_jitter(width = 0.07, alpha = 0.7) +
  stat_summary(geom = "point", shape = 3, size = 5,
               fun = "mean", show.legend = NA) +
  scale_colour_viridis_d(option = "D", begin = 0.2, 
                       end = 0.8, guide = "none") +
  scale_size_continuous(range = c(2,10), 
                        breaks = c(seq(0,200, by = 50), 1000)) +
  labs(x = "Author type", y = "Proportion of days\nwith pinniped interactions",
       size = "Total number\nof days") +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank())

# Species
spp_int <- propint_dat %>% 
  mutate(spp = gsub(" ", "\n", spp)) %>% 
  ggplot(aes(x = spp, y = x_int, size = n_int_days, colour = spp)) +
  geom_point(alpha = 0.7) +
  stat_summary(geom = "point", shape = 3, size = 5,
               fun = "mean", show.legend = NA) +
  scale_colour_viridis_d(option = "D", begin = 0.2, 
                         end = 0.8, guide = "none") +
  scale_size_continuous(range = c(2,10), 
                        breaks = c(seq(0,200, by = 50), 1000)) +
  labs(x = "Pinniped species", y = "Proportion of events\nwith pinniped interactions",
       size = "Total number\nof events") +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(size = 7))

# prevention
prev_int <- propint_dat %>% 
  mutate(prev = if_else(prev == 1, "Yes", "No")) %>% 
  ggplot(aes(x = prev, y = x_int, size = n_int_days)) +
  geom_jitter(width = 0.07, alpha = 0.7, 
              colour = viridis::viridis(20)[7]) +
  stat_summary(geom = "point", shape = 3, size = 5,
               fun = "mean", show.legend = NA) +
  scale_size_continuous(range = c(3,10), 
                        breaks = c(seq(0,200, by = 50), 1000)) +
  labs(x = "Prevention method?", y = "Proportion of events\nwith pinniped interactions",
       size = "Total number\nof events") +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank())

# method
method_int <- propint_dat %>% 
  mutate(method = case_when(method == "obs_onboard" ~ "onboard observation",
                            method == "survey; obs_onboard" ~ "survey + onboard",
                            TRUE ~ method)) %>% 
  ggplot(aes(x = method, y = x_int, size = n_int_days, colour = method)) +
  geom_jitter(width = 0.07, alpha = 0.7) +
  stat_summary(geom = "point", shape = 3, size = 5,
               fun = "mean", show.legend = NA) +
  scale_colour_viridis_d(option = "D", begin = 0.2, 
                         end = 0.8, guide = "none") +
  scale_size_continuous(range = c(3,10), 
                        breaks = c(seq(0,200, by = 50), 1000)) +
  labs(x = "Study method", y = "Proportion of events\nwith pinniped interactions",
       size = "Total number\nof events") +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank())

# fishing type
fishtype_int <- propint_dat %>% 
  ggplot(aes(x = fish_type, y = x_int, size = n_int_days, colour = fish_type)) +
  geom_jitter(width = 0.07, alpha = 0.7) +
  stat_summary(geom = "point", shape = 3, size = 5,
               fun = "mean", show.legend = NA) +
  scale_colour_viridis_d(option = "D", begin = 0.2, 
                         end = 0.8, guide = "none") +
  scale_size_continuous(range = c(3,10), 
                        breaks = c(seq(0, 200, by = 50), 1000)) +
  labs(x = "Fishery type", y = "Proportion of events\nwith pinniped interactions",
       size = "Total number\nof events") +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank())
  
# fishing location
fishloc_int <- propint_dat %>% 
  ggplot(aes(x = fish_loc, y = x_int, size = n_int_days, colour = fish_loc)) +
  geom_jitter(width = 0.07, alpha = 0.7) +
  stat_summary(geom = "point", shape = 3, size = 5,
               fun = "mean", show.legend = NA) +
  scale_colour_viridis_d(option = "D", begin = 0.2, 
                         end = 0.8, guide = "none") +
  scale_size_continuous(range = c(3,10), 
                        breaks = c(seq(0,200, by = 50), 1000)) +
  labs(x = "Observation location (relative to shore)", 
       y = "Proportion of events\nwith pinniped interactions",
       size = "Total number\nof events") +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank())


authtype_int + spp_int /
  method_int + prev_int /
  fishloc_int + fishtype_int

ggsave(authtype_int + spp_int /
         method_int + prev_int /
         fishloc_int + fishtype_int,
       filename = "../../Figures/extraction_20211209/prelim_binomial_interactions_rawplots.jpeg",
       width = 37, height = 30, units = "cm", dpi = 1000)


# Effort categories
effort_plot <- propint_dat %>% 
  mutate(effort_category = factor(effort_category, 
                                  levels = c("individual", 
                                             "journey/trip/trawl-session",
                                             "trap/net/set", "hook"))) %>% 
  ggplot(aes(x = effort_category, y = x_int, 
                        fill = effort_category)) +
  geom_boxplot(width = 0.2) +
  geom_point(aes(size = n_int_days)) +
  stat_summary(geom = "point", shape = 3, size = 5,
               fun = "mean", show.legend = NA) +
  scale_size_continuous(name = "Total\nobservations",
                        range = c(1,10), breaks = c(0,50,100,250,500,750)) +
  scale_fill_viridis_d(option = "B", begin = 0.4, end = 0.8, 
                       guide = "none") +
  labs(x = "Effort estimation category", y = "Proportion of observations\nwith Pinniped interactions") +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank())

ggsave(effort_plot, filename = "../../Figures/extraction_20211209/effort_interaction_plot_03022022.jpeg",
       width = 22, height = 13, units = "cm", dpi = 1000)

##______________________________________________________________________________
#### 3. Meta-analysis preliminary ####

# adding in effort categories
propint_dat <- filter(propint_dat, effort_category != "individual")

# The meta analysis in metafor
int_mod_base <- metaprop(pinn_interactions, total_interactions, 
                         studlab = acc_no, sm = "PLOGIT", data = propint_dat, 
                         method="Inverse", method.tau="DL")

summary(int_mod_base)

# forest plot to show accession contributions
jpeg("../../Figures/extraction_20211209/meta_regression_pinn_interactions_summary.jpeg",
     width = 35, height = 14, units = "cm", res = 1000)
forest.meta(int_mod_base, layout="RevMan5", xlab="Proportion", 
            comb.r=T, comb.f=F, xlim = c(0,1), fontsize=10, digits=3)
dev.off()


int_mod_base <- metaprop(pinn_interactions, total_interactions, 
                         studlab = acc_no, sm = "PLOGIT", data = propint_dat, 
                         method="Inverse", method.tau="DL")

summary(int_mod_base)

##______________________________________________________________________________
#### 4. Meta-regression with other factors ###

mod_effort_cat <- metareg(int_mod_base, ~ effort_category)







