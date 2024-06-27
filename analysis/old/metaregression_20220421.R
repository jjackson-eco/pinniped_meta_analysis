#####################################################
##                                                 ##
##   Pinniped Fisheries Interaction Meta-Analysis  ##
##                                                 ##
##             Binomial Meta-analysis              ##
##                                                 ##
##              JJ- April 21st 2022                ##
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

## Operational interaction data - non survey results with normalising constants
op_interaction <- pinnrev %>% 
  filter(method != "survey" &
           is.na(norm_int) == FALSE) %>% 
  mutate(interaction_prop = x_int,
         interaction_trials = n_int*norm_int,
         damage_prop = as.numeric(x_dmg),
         damage_trials = as.numeric(n_dmg)*norm_int,
         location = case_when(
           fish_loc == "off" ~ "Off shore",
           fish_loc == "near" ~ "Near shore",
           TRUE ~ "both"),
         fish_gear = case_when(
           fish_gear == "gillnet" ~ "net",
           fish_gear == "net, line" ~ "net + line",
           TRUE ~ fish_gear),
         fish_type = stringr::str_to_sentence(fish_type)) %>% 
  # Adding in events
  mutate(interaction_events = floor(interaction_prop*interaction_trials),
         damage_events = floor(damage_prop*damage_trials)) %>% 
  dplyr::select(acc_no, study, score, obs_no, author_type = auth_type,
                country = country, prevention = prev, compensation = comp,
                retaliation = retal, species = spp, pop_trend, fishery_type = fish_type,
                gear_type = fish_gear, location, 
                interaction_prop, interaction_events, interaction_trials,
                damage_prop, damage_events, damage_trials)
glimpse(op_interaction)

## corrections to study names
correction_studies <- data.frame(acc_no = c("acc_072", "acc_139", "acc_166", "acc_222",
                                            "acc_234", "acc_236", "acc_243", "acc_250", 
                                            "acc_270"),
                                 study_correct = c("Fjalling et al. 2006", "Konigson et al. 2009",
                                                   "Maravilla-Chavez et al. 2006",
                                                   "Rios et al. 2017", "Sepulveda et al. 2018",
                                                   "Sepulveda et al. 2007", "Soffker et al. 2015",
                                                   "Szteren and Paez 2002", "Wickens and Sim 1994") )

op_interaction <- op_interaction %>% 
  left_join(x = ., y = correction_studies, by = "acc_no") %>% 
  mutate(study = if_else(is.na(study_correct) == FALSE, study_correct, study)) %>% 
  dplyr::select(-study_correct)

glimpse(op_interaction)

save(op_interaction, file = "../../Data/op_interaction.RData")

##______________________________________________________________________________
#### 2. Meta-analyses ####

#__________________________________________________________
## 2a. filtering for complete cases for both dependent vars
int_dat <- filter(op_interaction, is.na(interaction_events) == FALSE)
dmg_dat <- filter(op_interaction, is.na(damage_events) == FALSE)

#__________________________________
## 2b. The meta analyses in metafor
int_mod_base <- metaprop(interaction_events, interaction_trials, 
                         studlab = study, sm = "PLOGIT", data = int_dat, 
                         method="Inverse", method.tau="DL")

dmg_mod_base <- metaprop(damage_events, damage_trials, 
                         studlab = study, sm = "PLOGIT", data = dmg_dat, 
                         method="Inverse", method.tau="DL")
#__________________________________
## 2c. Model summaries
int_mod_base_summary <- summary(int_mod_base)

int_pred <- tibble(study = int_mod_base_summary$studlab,
                   n = int_mod_base_summary$n,
                   pred = boot::inv.logit(int_mod_base_summary$TE),
                   lwr = int_mod_base_summary$lower,
                   upr = int_mod_base_summary$upper)

int_pred_ovr <- tibble(mod = c("Fixed effect only", "Random effects model"),
                       pred = c(boot::inv.logit(int_mod_base_summary$TE.fixed),
                                boot::inv.logit(int_mod_base_summary$TE.random)),
                       lwr = c(boot::inv.logit(int_mod_base_summary$lower.fixed),
                               boot::inv.logit(int_mod_base_summary$lower.random)),
                       upr = c(boot::inv.logit(int_mod_base_summary$upper.fixed),
                               boot::inv.logit(int_mod_base_summary$upper.random)))
  

dmg_mod_base_summary <- summary(dmg_mod_base)

dmg_pred <- tibble(study = dmg_mod_base_summary$studlab,
                   n = dmg_mod_base_summary$n,
                   pred = boot::inv.logit(dmg_mod_base_summary$TE),
                   lwr = dmg_mod_base_summary$lower,
                   upr = dmg_mod_base_summary$upper)

dmg_pred_ovr <- tibble(mod = c("Fixed effect only", "Random effects model"),
                       pred = c(boot::inv.logit(dmg_mod_base_summary$TE.fixed),
                                boot::inv.logit(dmg_mod_base_summary$TE.random)),
                       lwr = c(boot::inv.logit(dmg_mod_base_summary$lower.fixed),
                               boot::inv.logit(dmg_mod_base_summary$lower.random)),
                       upr = c(boot::inv.logit(dmg_mod_base_summary$upper.fixed),
                               boot::inv.logit(dmg_mod_base_summary$upper.random)))

#__________________________________
## 2c. Summary table plots

int_colour <- MetPalettes$Hokusai2[[1]][3]
dmg_colour <- MetPalettes$Hokusai2[[1]][6]

# Interaction
int_plot <- ggplot(int_pred, aes(x = pred, y = study)) +
  geom_vline(xintercept = int_pred_ovr$pred[2], 
             colour = int_colour) +
  geom_errorbar(aes(xmax = upr, xmin = lwr), 
                width = 0, colour = int_colour,
                size = 1) +
  geom_point(aes(size = n), colour = int_colour) +
  annotate(geom = "rect", xmin = int_pred_ovr$lwr[2], 
           xmax = int_pred_ovr$upr[2], ymin = -0.1, ymax = 19, 
           fill = int_colour, alpha = 0.07) +
  scale_size_continuous(trans = "log10", breaks = c(1,10,100,1000,5000),
                        limits = c(1,5000), guide = "none", range = c(2,8)) +
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,by = 0.1)) +
  labs(x = NULL, y = NULL,
       tag = "a)") +
  theme_bw(base_size = 16) +
  theme(panel.grid = element_blank())

int_ovr_plot <- ggplot(int_pred_ovr, aes(x = pred, y = mod)) +
  geom_vline(xintercept = int_pred_ovr$pred[2], 
             colour = int_colour) +
  geom_errorbar(aes(xmax = upr, xmin = lwr), 
                width = 0, colour = int_colour,
                size = 1) +
  geom_point(size = 6, colour = int_colour) +
  annotate(geom = "rect", xmin = int_pred_ovr$lwr[2], 
           xmax = int_pred_ovr$upr[2], ymin = -0.1, ymax = 3, 
           fill = int_colour, alpha = 0.07) +
  annotate("text", label = paste0(round(int_pred_ovr$pred[2]*100, 1),"%"),
           x = 0.44, y = 2.4, size = 6) +
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,by = 0.1)) +
  labs(x = "Predicted proportion of days with interactions", y = NULL) +
  theme_bw(base_size = 16) +
  theme(panel.grid = element_blank())

# Damage
dmg_plot <- ggplot(dmg_pred, aes(x = pred, y = study)) +
    geom_vline(xintercept = dmg_pred_ovr$pred[2], 
               colour = dmg_colour) +
    geom_errorbar(aes(xmax = upr, xmin = lwr), 
                  width = 0, colour = dmg_colour,
                  size = 1) +
    geom_point(aes(size = n), colour = dmg_colour) +
    annotate(geom = "rect", xmin = dmg_pred_ovr$lwr[2], 
             xmax = dmg_pred_ovr$upr[2], ymin = -0.1, ymax = 20, 
             fill = dmg_colour, alpha = 0.07) +
    scale_size_continuous(trans = "log10", breaks = c(1,10,100,1000,5000),
                          limits = c(1,5000), range = c(2,8)) +
    scale_x_continuous(limits = c(0,1), breaks = seq(0,1,by = 0.1)) +
    guides(size = guide_legend(override.aes = list(colour = "black"))) +
    labs(x = NULL, y = NULL,
         size = "Number of\nfishing days", tag = "b)") +
    theme_bw(base_size = 16) +
    theme(panel.grid = element_blank())

dmg_ovr_plot <- ggplot(dmg_pred_ovr, aes(x = pred, y = mod)) +
  geom_vline(xintercept = dmg_pred_ovr$pred[2], 
             colour = dmg_colour) +
  geom_errorbar(aes(xmax = upr, xmin = lwr), 
                width = 0, colour = dmg_colour,
                size = 1) +
  geom_point(size = 6, colour = dmg_colour) +
  annotate(geom = "rect", xmin = dmg_pred_ovr$lwr[2], 
           xmax = dmg_pred_ovr$upr[2], ymin = -0.1, ymax = 3, 
           fill = dmg_colour, alpha = 0.07) +
  annotate("text", label = paste0(round(dmg_pred_ovr$pred[2]*100, 1),"%"),
           x = 0.22, y = 2.4, size = 6) +
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,by = 0.1)) +
  labs(x = "Predicted proportion of catch lost", y = NULL) +
  theme_bw(base_size = 16) +
  theme(panel.grid = element_blank())

# Heterogeneity plots
het_int <- tibble(Q = paste0(round(int_mod_base$Q, 1), " (p < 0.001)"),
       I2 = paste0(round(int_mod_base$I2, 3)*100, "% [", 
                   round(int_mod_base$lower.I2, 3)*100, ":",
                   round(int_mod_base$upper.I2, 3)*100, "]"),
       tau2 = paste0(round(int_mod_base$tau2, 1), " [", 
                     round(int_mod_base$lower.tau2, 1), ":",
                     round(int_mod_base$upper.tau2, 1), "]"),
       H = paste0(round(int_mod_base$H, 1), " [", 
              round(int_mod_base$lower.H, 1), ":",
              round(int_mod_base$upper.H, 1), "]")) %>% 
  flextable(cwidth = 1.4) %>% 
  compose(part = "header", j = "Q", value = as_paragraph("Cochran's Q")) %>% 
  compose(part = "header", j = "I2", value = as_paragraph("I", as_sup("2"))) %>% 
  compose(part = "header", j = "tau2", value = as_paragraph("\U03C4", as_sup("2"))) %>%
  add_header_row(x = ., values = c("Interaction heterogeneity"),colwidths = 4) %>% 
  theme_zebra(odd_header = alpha(int_colour, alpha = 0.3)) %>% 
  save_as_image("../../Figures/extraction_20220512/interaction_heterogeneity.png")

het_dmg <- tibble(Q = paste0(round(dmg_mod_base$Q, 1), " (p < 0.001)"),
                  I2 = paste0(round(dmg_mod_base$I2, 3)*100, "% [", 
                              round(dmg_mod_base$lower.I2, 3)*100, ":",
                              round(dmg_mod_base$upper.I2, 3)*100, "]"),
                  tau2 = paste0(round(dmg_mod_base$tau2, 1), " [", 
                                round(dmg_mod_base$lower.tau2, 1), ":",
                                round(dmg_mod_base$upper.tau2, 1), "]"),
                  H = paste0(round(dmg_mod_base$H, 1), " [", 
                             round(dmg_mod_base$lower.H, 1), ":",
                             round(dmg_mod_base$upper.H, 1), "]")) %>% 
  flextable(cwidth = 1.4) %>% 
  compose(part = "header", j = "Q", value = as_paragraph("Cochran's Q")) %>% 
  compose(part = "header", j = "I2", value = as_paragraph("I", as_sup("2"))) %>% 
  compose(part = "header", j = "tau2", value = as_paragraph("\U03C4", as_sup("2"))) %>%
  add_header_row(x = ., values = c("Damage heterogeneity"),colwidths = 4) %>% 
  theme_zebra(odd_header = alpha(dmg_colour, alpha = 0.3)) %>% 
  save_as_image("../../Figures/extraction_20220512/damage_heterogeneity.png")



layout_fig1 <- "
AABB
AABB
AABB
AABB
AABB
CCDD
"

fig1 <- wrap_plots(A = int_plot, 
                   B = dmg_plot,
                   C = int_ovr_plot, 
                   D = dmg_ovr_plot,
                   design = layout_fig1) 

ggsave(fig1, filename = "../../Figures/extraction_20220512/meta_analysis_summary.pdf",
       width = 41, height = 33, units = "cm")

##______________________________________________________________________________
#### 3. Meta-regressions ####

#_______________________________________________
## 3a. Interactions + fishery size and location

# raw data
ggplot(int_dat, aes(x = fishery_type, y = interaction_prop)) +
  geom_jitter(width = 0.1, colour = int_colour)

ggplot(int_dat, aes(x = gear_type, y = interaction_prop)) +
  geom_jitter(width = 0.1, colour = int_colour)

# X tabulation - separation so doing them separately
table(int_dat$fishery_type, int_dat$gear_type) 

# Models
int_fishery <- metareg(int_mod_base, ~ fishery_type)
# int_location <- metareg(int_mod_base, ~ location) #Same as fishery type more or less
int_gear <- metareg(int_mod_base, ~ gear_type)

# Summaries
int_fishery_summary <- summary(int_fishery)
int_gear_summary <- summary(int_gear)

int_gear_summary$fit.stats
int_fishery_summary$fit.stats

# predictions
library(boot)
int_fish_pred_ovr <- tibble(fishery_type = c("Large", "Small"),
                            pred = c(inv.logit(int_fishery_summary$beta[1]),
                                     inv.logit(int_fishery_summary$beta[1] + 
                                           int_fishery_summary$beta[2])),
                            lwr = c(inv.logit(int_fishery_summary$ci.lb[1]),
                                    inv.logit(int_fishery_summary$beta[1] + 
                                                int_fishery_summary$ci.lb[2])),
                            upr = c(inv.logit(int_fishery_summary$ci.ub[1]),
                                    inv.logit(int_fishery_summary$beta[1] + 
                                                int_fishery_summary$ci.ub[2])))

int_gear_pred_ovr <- tibble(gear_type = c("line", "net", "net + line", "other", "trawl"),
                            pred = c(inv.logit(int_gear_summary$beta[1]),
                                     inv.logit(int_gear_summary$beta[1] + 
                                                 int_gear_summary$beta[2:5])),
                            lwr = c(inv.logit(int_gear_summary$ci.lb[1]),
                                    inv.logit(int_gear_summary$beta[1] + 
                                                int_gear_summary$ci.lb[2:5])),
                            upr = c(inv.logit(int_gear_summary$ci.ub[1]),
                                    inv.logit(int_gear_summary$beta[1] + 
                                                int_gear_summary$ci.ub[2:5])))

#_______________________________________________
## 3b. Plots

fig_3a <- ggplot(int_dat, aes(x = fishery_type, y = interaction_prop)) +
  geom_violin(fill = int_colour, alpha = 0.1, colour = "white") +
  geom_jitter(width = 0.07, colour = int_colour, size = 2, alpha = 0.8) +
  geom_errorbar(data = int_fish_pred_ovr, 
                aes(ymin = lwr, ymax = upr, y = NULL), 
                width = 0, size = 1.1) +
  geom_point(data = int_fish_pred_ovr, aes(y = pred), 
             size = 4) +
  geom_segment(aes(x = 1, xend = 2, y = 1.04, yend = 1.04), size = 0.6) +
  geom_segment(aes(x = 1, xend = 1, y = 1.04, yend = 1.02), size = 0.6) +
  geom_segment(aes(x = 2, xend = 2, y = 1.04, yend = 1.02), size = 0.6) +
  annotate(geom = "text", label = "**", x = 1.5, y = 1.06, size = 6) +
  labs(x = "Fishery size", y = "Proportion of days with interactions", tag = "a)") +
  scale_y_continuous(breaks = seq(0,1, by = 0.2), limits = c(0,1.1)) +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank())

fig_3b <- ggplot(int_dat, aes(x = gear_type, y = interaction_prop)) +
  geom_violin(fill = int_colour, alpha = 0.1, colour = "white") +
  geom_jitter(width = 0.07, colour = int_colour, size = 2, alpha = 0.8) +
  geom_errorbar(data = filter(int_gear_pred_ovr, 
                              gear_type %in% c("net + line", "other") == FALSE), 
                aes(ymin = lwr, ymax = upr, y = NULL), 
                width = 0, size = 1.1) +
  geom_point(data = filter(int_gear_pred_ovr, 
                           gear_type %in% c("net + line", "other") == FALSE), 
             aes(y = pred), 
             size = 4) +
  annotate(geom = "text", label = "**", x = 2, y = 1.06, size = 6) +
  annotate(geom = "text", label = "*", x = 4, y = 1.06, size = 6) +
  labs(x = "Gear type", y = "Proportion of days with interactions", tag = "b)") +
  scale_y_continuous(breaks = seq(0,1, by = 0.2), limits = c(0,1.1)) +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank())

#_______________________________________________
## 3c. Damage + fishery size and location

# raw data
ggplot(dmg_dat, aes(x = fishery_type, y = damage_prop)) +
  geom_jitter(width = 0.1, colour = dmg_colour)

ggplot(dmg_dat, aes(x = gear_type, y = damage_prop)) +
  geom_jitter(width = 0.1, colour = dmg_colour)

# Models
dmg_fishery <- metareg(dmg_mod_base, ~ fishery_type)
dmg_gear <- metareg(dmg_mod_base, ~ gear_type)

# Summaries
dmg_fishery_summary <- summary(dmg_fishery)
dmg_gear_summary <- summary(dmg_gear)

dmg_gear_summary$fit.stats
dmg_fishery_summary$fit.stats

# predictions
library(boot)
dmg_fish_pred_ovr <- tibble(fishery_type = c("Large", "Small"),
                            pred = c(inv.logit(dmg_fishery_summary$beta[1]),
                                     inv.logit(dmg_fishery_summary$beta[1] + 
                                                 dmg_fishery_summary$beta[2])),
                            lwr = c(inv.logit(dmg_fishery_summary$ci.lb[1]),
                                    inv.logit(dmg_fishery_summary$beta[1] + 
                                                dmg_fishery_summary$ci.lb[2])),
                            upr = c(inv.logit(dmg_fishery_summary$ci.ub[1]),
                                    inv.logit(dmg_fishery_summary$beta[1] + 
                                                dmg_fishery_summary$ci.ub[2])))

dmg_gear_pred_ovr <- tibble(gear_type = c("line", "net", "net + line", "other", "trawl"),
                            pred = c(inv.logit(dmg_gear_summary$beta[1]),
                                     inv.logit(dmg_gear_summary$beta[1] + 
                                                 dmg_gear_summary$beta[2:5])),
                            lwr = c(inv.logit(dmg_gear_summary$ci.lb[1]),
                                    inv.logit(dmg_gear_summary$beta[1] + 
                                                dmg_gear_summary$ci.lb[2:5])),
                            upr = c(inv.logit(dmg_gear_summary$ci.ub[1]),
                                    inv.logit(dmg_gear_summary$beta[1] + 
                                                dmg_gear_summary$ci.ub[2:5])))

#_______________________________________________
## 3d. Plots

fig_3c <- ggplot(dmg_dat, aes(x = fishery_type, y = damage_prop)) +
  geom_violin(fill = dmg_colour, alpha = 0.1, colour = "white") +
  geom_jitter(width = 0.07, colour = dmg_colour, size = 2, alpha = 0.8) +
  geom_errorbar(data = dmg_fish_pred_ovr, 
                aes(ymin = lwr, ymax = upr, y = NULL), 
                width = 0, size = 1.1) +
  geom_point(data = dmg_fish_pred_ovr, aes(y = pred), 
             size = 4) +
  geom_segment(aes(x = 1, xend = 2, y = 0.79, yend = 0.79), size = 0.6) +
  geom_segment(aes(x = 1, xend = 1, y = 0.79, yend = 0.77), size = 0.6) +
  geom_segment(aes(x = 2, xend = 2, y = 0.79, yend = 0.77), size = 0.6) +
  annotate(geom = "text", label = "*", x = 1.5, y = 0.81, size = 6) +
  labs(x = "Fishery size", y = "Proportion of catch lost", tag = "c)") +
  scale_y_continuous(breaks = seq(0,1, by = 0.2), limits = c(0,1)) +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank())

fig_3d <- ggplot(dmg_dat, aes(x = gear_type, y = damage_prop)) +
  geom_violin(fill = dmg_colour, alpha = 0.1, colour = "white") +
  geom_jitter(width = 0.07, colour = dmg_colour, size = 2, alpha = 0.8) +
  geom_errorbar(data = filter(dmg_gear_pred_ovr, 
                              gear_type %in% c("net + line", "other") == FALSE), 
                aes(ymin = lwr, ymax = upr, y = NULL), 
                width = 0, size = 1.1) +
  geom_point(data = filter(dmg_gear_pred_ovr, 
                           gear_type %in% c("net + line", "other") == FALSE), 
             aes(y = pred), 
             size = 4) +
  annotate(geom = "text", label = "**", x = 2, y = 0.81, size = 6) +
  annotate(geom = "text", label = "***", x = 3, y = 0.81, size = 6) +
  labs(x = "Gear type", y = "Proportion of catch lost", tag = "d)") +
  scale_y_continuous(breaks = seq(0,1, by = 0.2), limits = c(0,1)) +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank())


ggsave((fig_3a + fig_3b)/ (fig_3c + fig_3d), filename = "../../Figures/extraction_20220512/fig3.pdf",
       width = 30, height = 25, units = "cm")

ggsave((fig_3a + fig_3b)/ (fig_3c + fig_3d), filename = "../../Figures/extraction_20220512/fig3.jpeg",
       width = 30, height = 25, units = "cm", dpi = 1300)


##______________________________________________________________________________
#### 4. Other covariates ####

pop_int <- ggplot(int_dat, aes(x = pop_trend, y = interaction_prop)) +
  geom_jitter(width = 0.05, colour = int_colour, size = 5, alpha = 0.8) +
  labs(x = "Population trend", y = "Proportion of days with interactions") +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank())

pop_dmg <- ggplot(dmg_dat, aes(x = pop_trend, y = damage_prop)) +
  geom_jitter(width = 0.05, colour = dmg_colour, size = 5, alpha = 0.8) +
  labs(x = "Population trend", y = "Proportion of catch lost") +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank())

ggsave(pop_int / pop_dmg, filename = "../../Figures/extraction_20220512/pop_int_dmg.pdf",
       width = 10, height = 12)

sp_int <- ggplot(int_dat, aes(x = species, y = interaction_prop)) +
  geom_point(colour = int_colour, size = 6, alpha = 0.8)+
  labs(x = "Species", y = "Proportion of days with interactions") +
  coord_flip() +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank())

sp_dmg <- ggplot(dmg_dat, aes(x = species, y = damage_prop)) +
  geom_point(colour = dmg_colour, size = 6, alpha = 0.8)+
  labs(x = "Species", y = "Proportion of catch lost") +
  coord_flip() +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank())

ggsave(sp_int /sp_dmg, filename = "../../Figures/extraction_20220512/species_int_dmg.pdf",
      width = 10, height = 17)


### Location

fig_S1a <- ggplot(int_dat, aes(x = location, y = interaction_prop)) +
  geom_violin(fill = int_colour, alpha = 0.1, colour = "white") +
  geom_jitter(width = 0.07, colour = int_colour, size = 2, alpha = 0.8) +
  labs(x = "Location of operations", y = "Proportion of days with interactions", tag = "a)") +
  scale_y_continuous(breaks = seq(0,1, by = 0.2), limits = c(0,1.1)) +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank())


fig_S1b <- ggplot(dmg_dat, aes(x = location, y = damage_prop)) +
  geom_violin(fill = dmg_colour, alpha = 0.1, colour = "white") +
  geom_jitter(width = 0.07, colour = dmg_colour, size = 2, alpha = 0.8) +
  labs(x = "Location of operations", y = "Proportion of catch lost", tag = "b)") +
  scale_y_continuous(breaks = seq(0,1, by = 0.2), limits = c(0,1)) +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank())

ggsave(fig_S1a + fig_S1b, filename = "../../Figures/extraction_20220512/figS1.jpeg",
       width =25, height = 12.5, units = "cm", dpi = 1300)

##______________________________________________________________________________
#### 5. Summary stats ####

sum(int_dat$interaction_trials) + sum(dmg_dat$damage_trials)

n_distinct(op_interaction$acc_no)
n_distinct(int_dat$acc_no)
n_distinct(dmg_dat$acc_no)

nrow(int_dat)
nrow(dmg_dat)

summary(op_interaction$interaction_trials)
summary(op_interaction$damage_trials)

sum(op_interaction$interaction_trials, na.rm = T)
sum(op_interaction$damage_trials, na.rm = T)

summary(op_interaction$interaction_prop)
summary(op_interaction$damage_prop)

int_mod_base
dmg_mod_base

int_fishery
dmg_fishery

int_fish_pred_ovr
dmg_fish_pred_ovr

int_gear_pred_ovr
dmg_gear_pred_ovr

pinnrev %>% group_by(acc_no) %>% 
  summarise(country = country[1]) %>% 
  as.data.frame()



