#####################################################
##                                                 ##
##   Pinniped Fisheries Interaction Meta-Analysis  ##
##                                                 ##
##             Binomial Meta-analysis              ##
##                                                 ##
##              JJ- August 24th 2023               ##
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

## Interaction data
pinnrev <- read_csv("../../Data/scoping_data_extraction_20230824.csv") %>% 
  mutate(spp = paste0(genus, " ", species)) %>% 
  ## Standardising study names
  mutate(study = unlist(lapply(X = 1:nrow(.), function(x){
    crow = .[x,]
    
    # lead + second
    c_lead = unlist(str_split(crow$author,pattern = ","))[1] %>% 
      gsub(x = ., pattern = "[[:space:]].*", replacement = "")
    
    c_second = unlist(str_split(crow$author,pattern = ","))[2] %>%
      str_trim(., side = "left") %>% 
      gsub(x = ., pattern = "[[:space:]].*", replacement = "")
    
    # number of authors
    n_auth <- length(unlist(str_split(crow$author,pattern = ",")))
    
    # generate study based on number of authors
    if(n_auth == 1){study = paste0(c_lead, " ", crow$date)}
    else if(n_auth == 2){study = paste0(c_lead, " and ", c_second, " ", crow$date)}
    else{study = paste0(c_lead, " et al. ", crow$date)}
    
    # return
    return(study)})))
glimpse(pinnrev)

## Unicode and checked names for labels
uni_studies <- read_csv("../../Data/study_names_unicode_20221122.csv") %>% 
  group_by(acc_no) %>% 
  summarise(across(.fns = first)) %>% 
  ungroup() %>% 
  filter(grepl("\\\\", study_uni)) %>%
  ## Has to be added within R
  mutate(study_uni = c("Fj\U00E4lling et al. 2006",
                       paste0("H\U00FC", "ckst\U00E4", "dt and Antezana 2003"),
                       "K\U00F6nigson et al. 2009", 
                       "Maravilla-Ch\U00E1vez et al. 2006",
                       "R\U00EDos et al. 2017",
                       "Sep\U00FAlveda et al. 2018",
                       "Sep\U00FAlveda et al. 2007",
                       paste0("S\U00F6","ffker et al. 2015"),
                       paste0("Szteren and P", "\U00E1", "ez 2002"),
                       paste0("Me", "\U00FF", "er et al. 1992")))

corrected_study_names <- 
  read_csv("../../Data/study_names_unicode_20221121.csv") %>% 
  group_by(acc_no) %>% 
  summarise(across(.fns = first)) %>% 
  ungroup() %>% 
  filter(grepl("\\\\", study_uni) == F) %>% 
  bind_rows(uni_studies)

## Operational interaction data - non survey results with normalising constants
op_interaction <- pinnrev %>% 
  # these two filtering steps shouldn't apply to the new data - but just checking
  filter(method != "survey" &
           is.na(x_norm) == FALSE) %>% 
  left_join(dplyr::select(corrected_study_names, c(1,3)), by = "acc_no") %>% 
  mutate(interaction_prop = x_int,
         interaction_trials = n_int*x_norm,
         damage_prop = as.numeric(x_dmg),
         damage_trials = as.numeric(n_dmg)*x_norm,
         location = case_when(
           fish_loc == "off" ~ "Off shore",
           fish_loc == "near" ~ "Near shore",
           TRUE ~ "both"),
         fish_gear = case_when(
           fish_gear == "gillnet" ~ "net",
           fish_gear == "net, line" ~ "other",
           TRUE ~ fish_gear),
         fish_type = stringr::str_to_sentence(fish_type),
         study_lab = if_else(is.na(study_uni) == T, study, study_uni)) %>% 
  # Adding in events
  mutate(interaction_events = floor(interaction_prop*interaction_trials),
         damage_events = floor(damage_prop*damage_trials)) %>% 
  dplyr::select(acc_no, study, study_lab, doi, obs_no, author_type = auth_type,
                country = country, prevention = prev, compensation = comp,
                retaliation = retal, species = spp, pop_trend, fishery_type = fish_type,
                gear_type = fish_gear, location, 
                interaction_prop, interaction_events, interaction_trials,
                damage_prop, damage_events, damage_trials)
glimpse(op_interaction)

# save(op_interaction, file = "../../Data/op_interaction.RData")

##______________________________________________________________________________
#### 2. Meta-analyses ####

#__________________________________________________________
## 2a. filtering for complete cases for both dependent vars
int_dat <- filter(op_interaction, is.na(interaction_events) == FALSE)
dmg_dat <- filter(op_interaction, is.na(damage_events) == FALSE)

#__________________________________
## 2b. The meta analyses in metafor
int_mod_base <- metaprop(interaction_events, interaction_trials, 
                         studlab = study_lab, sm = "PLOGIT", data = int_dat, 
                         method="Inverse", method.tau="DL")

dmg_mod_base <- metaprop(damage_events, damage_trials, 
                         studlab = study_lab, sm = "PLOGIT", data = dmg_dat, 
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
           xmax = int_pred_ovr$upr[2], ymin = -0.1, ymax = 25, 
           fill = int_colour, alpha = 0.07) +
  scale_size_continuous(trans = "log10", breaks = c(1,10,100,1000,5000, 25000),
                        limits = c(1,25000), guide = "none", range = c(2,8)) +
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,by = 0.1)) +
  labs(x = NULL, y = NULL,
       tag = "a)") +
  theme_bw(base_size = 16) +
  theme(panel.grid = element_blank(),
        axis.text.y = element_text(face = "italic"))

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
           xmax = dmg_pred_ovr$upr[2], ymin = -0.1, ymax = 23, 
           fill = dmg_colour, alpha = 0.07) +
  scale_size_continuous(trans = "log10", breaks = c(1,10,100,1000,5000),
                        limits = c(0.1,5000), range = c(2,8)) +
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,by = 0.1)) +
  guides(size = guide_legend(override.aes = list(colour = "black"))) +
  labs(x = NULL, y = NULL,
       size = "Number of\nfishing days", tag = "b)") +
  theme_bw(base_size = 16) +
  theme(panel.grid = element_blank(),
        axis.text.y = element_text(face = "italic"))

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
           x = 0.24, y = 2.4, size = 6) +
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
  save_as_image("../../Figures/extraction_20230112/interaction_heterogeneity.png")

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
  save_as_image("../../Figures/extraction_20230112/damage_heterogeneity.png")



layout_fig2 <- "
AABB
AABB
AABB
AABB
AABB
CCDD
"

fig2 <- wrap_plots(A = int_plot, 
                   B = dmg_plot,
                   C = int_ovr_plot, 
                   D = dmg_ovr_plot,
                   design = layout_fig2) 

ggsave(fig2, filename = "../../Figures/meta_analysis_summary.pdf",
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

int_gear_pred_ovr <- tibble(gear_type = c("line", "net", "other", "trawl"),
                            pred = c(inv.logit(int_gear_summary$beta[1]),
                                     inv.logit(int_gear_summary$beta[1] + 
                                                 int_gear_summary$beta[2:4])),
                            lwr = c(inv.logit(int_gear_summary$ci.lb[1]),
                                    inv.logit(int_gear_summary$beta[1] + 
                                                int_gear_summary$ci.lb[2:4])),
                            upr = c(inv.logit(int_gear_summary$ci.ub[1]),
                                    inv.logit(int_gear_summary$beta[1] + 
                                                int_gear_summary$ci.ub[2:4])))

#_______________________________________________
## 3b. Plots

fig_3a <- ggplot(int_dat, aes(x = fishery_type, y = interaction_prop)) +
  geom_violin(fill = int_colour, alpha = 0.1, colour = "white") +
  geom_jitter(width = 0.07, colour = int_colour, size = 3.5, alpha = 0.65) +
  geom_errorbar(data = int_fish_pred_ovr, 
                aes(ymin = lwr, ymax = upr, y = NULL), 
                width = 0, size = 1.1) +
  geom_point(data = int_fish_pred_ovr, aes(y = pred), 
             size = 5) +
  geom_segment(aes(x = 1, xend = 2, y = 1.04, yend = 1.04), size = 0.6) +
  geom_segment(aes(x = 1, xend = 1, y = 1.04, yend = 1.02), size = 0.6) +
  geom_segment(aes(x = 2, xend = 2, y = 1.04, yend = 1.02), size = 0.6) +
  annotate(geom = "text", label = "**", x = 1.5, y = 1.07, size = 8) +
  labs(x = "Fishery scale", y = "Proportion of days with interactions", tag = "a)") +
  scale_y_continuous(breaks = seq(0,1, by = 0.2), limits = c(0,1.3)) +
  theme_bw(base_size = 15) +
  theme(panel.grid = element_blank())

fig_3b <- ggplot(int_dat, aes(x = gear_type, y = interaction_prop)) +
  geom_violin(fill = int_colour, alpha = 0.1, colour = "white") +
  geom_jitter(width = 0.07, colour = int_colour, size = 3.5, alpha = 0.65) +
  geom_errorbar(data = int_gear_pred_ovr, 
                aes(ymin = lwr, ymax = upr, y = NULL), 
                width = 0, size = 1.1) +
  geom_point(data = int_gear_pred_ovr, aes(y = pred), 
             size = 5) +
  annotate(geom = "text", label = "*", x = 2, y = 1.06, size = 8) +
  annotate(geom = "text", label = "*", x = 3, y = 1.06, size = 8) +
  labs(x = "Gear type", y = "Proportion of days with interactions", tag = "b)") +
  scale_y_continuous(breaks = seq(0,1, by = 0.2), limits = c(0,1.3)) +
  scale_x_discrete(labels = c("Line", "Net", "Other", "Trawl")) +
  theme_bw(base_size = 15) +
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

dmg_gear_pred_ovr <- tibble(gear_type = c("line", "net", "other"),
                            pred = c(inv.logit(dmg_gear_summary$beta[1]),
                                     inv.logit(dmg_gear_summary$beta[1] + 
                                                 dmg_gear_summary$beta[2:3])),
                            lwr = c(inv.logit(dmg_gear_summary$ci.lb[1]),
                                    inv.logit(dmg_gear_summary$beta[1] + 
                                                dmg_gear_summary$ci.lb[2:3])),
                            upr = c(inv.logit(dmg_gear_summary$ci.ub[1]),
                                    inv.logit(dmg_gear_summary$beta[1] + 
                                                dmg_gear_summary$ci.ub[2:3])))

#_______________________________________________
## 3d. Plots

fig_3c <- ggplot(dmg_dat, aes(x = fishery_type, y = damage_prop)) +
  geom_violin(fill = dmg_colour, alpha = 0.1, colour = "white") +
  geom_jitter(width = 0.07, colour = dmg_colour, size = 3.5, alpha = 0.65) +
  geom_errorbar(data = dmg_fish_pred_ovr, 
                aes(ymin = lwr, ymax = upr, y = NULL), 
                width = 0, size = 1.1) +
  geom_point(data = dmg_fish_pred_ovr, aes(y = pred), 
             size = 5) +
  geom_segment(aes(x = 1, xend = 2, y = 1.04, yend = 1.04), size = 0.6) +
  geom_segment(aes(x = 1, xend = 1, y = 1.04, yend = 1.02), size = 0.6) +
  geom_segment(aes(x = 2, xend = 2, y = 1.04, yend = 1.02), size = 0.6) +
  annotate(geom = "text", label = "*", x = 1.5, y = 1.07, size = 8) +
  labs(x = "Fishery scale", y = "Proportion of catch lost", tag = "c)") +
  scale_y_continuous(breaks = seq(0,1, by = 0.2), limits = c(-0.01,1.3)) +
  theme_bw(base_size = 15) +
  theme(panel.grid = element_blank())

fig_3d <- ggplot(dmg_dat, aes(x = gear_type, y = damage_prop)) +
  geom_violin(fill = dmg_colour, alpha = 0.1, colour = "white") +
  geom_jitter(width = 0.07, colour = dmg_colour, size = 3.5, alpha = 0.65) +
  geom_errorbar(data = dmg_gear_pred_ovr, 
                aes(ymin = lwr, ymax = upr, y = NULL), 
                width = 0, size = 1.1) +
  geom_point(data = dmg_gear_pred_ovr, 
             aes(y = pred), 
             size = 5) +
  annotate(geom = "text", label = "*", x = 2, y = 1.06, size = 8) +
  annotate(geom = "text", label = "**", x = 3, y = 1.06, size = 8) +
  labs(x = "Gear type", y = "Proportion of catch lost", tag = "d)") +
  scale_y_continuous(breaks = seq(0,1, by = 0.2), limits = c(-0.01,1.3)) +
  scale_x_discrete(labels = c("Line", "Net", "Other")) +
  theme_bw(base_size = 15) +
  theme(panel.grid = element_blank())


# ggsave((fig_3a + fig_3b)/ (fig_3c + fig_3d), filename = "../../Figures/extraction_20230112/fig3.pdf",
#        width = 30, height = 25, units = "cm")

ggsave((fig_3a + fig_3b)/ (fig_3c + fig_3d), filename = "../../Figures/Manuscript_figures/fig3_noicons.jpeg",
       width = 30, height = 25, units = "cm", dpi = 1300)

##______________________________________________________________________________
#### 4. Other covariates ####

pop_int <- ggplot(int_dat, aes(x = factor(pop_trend), y = interaction_prop)) +
  geom_violin(fill = int_colour, alpha = 0.1, colour = "white") +
  geom_jitter(width = 0.05, colour = int_colour, size = 3.5, alpha = 0.8) +
  labs(x = "Population trend", y = "Proportion of days with interactions") +
  theme_bw(base_size = 11) +
  theme(panel.grid = element_blank())

pop_dmg <- ggplot(dmg_dat, aes(x = factor(pop_trend), y = damage_prop)) +
  geom_violin(fill = dmg_colour, alpha = 0.1, colour = "white") +
  geom_jitter(width = 0.05, colour = dmg_colour, size = 3.5, alpha = 0.8) +
  labs(x = "Population trend", y = "Proportion of catch lost") +
  theme_bw(base_size = 11) +
  theme(panel.grid = element_blank())

ggsave(pop_int + pop_dmg, filename = "../../Figures/extraction_20230112/pop_int_dmg.pdf",
       width = 14, height = 8, units = "cm")

sp_int <- ggplot(int_dat, aes(x = species, y = interaction_prop)) +
  geom_violin(fill = int_colour, alpha = 0.1, colour = "white") +
  geom_point(colour = int_colour, size = 3.5, alpha = 0.8)+
  labs(x = "Species", y = "Proportion of days with interactions") +
  coord_flip() +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank())

sp_dmg <- ggplot(dmg_dat, aes(x = species, y = damage_prop)) +
  geom_violin(fill = dmg_colour, alpha = 0.1, colour = "white") +
  geom_point(colour = dmg_colour, size = 3.5, alpha = 0.8)+
  labs(x = "Species", y = "Proportion of catch lost") +
  coord_flip() +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank())

ggsave(sp_int / sp_dmg, 
       filename = "../../Figures/extraction_20230112/species_int_dmg.pdf",
       width = 16, height = 17, units = "cm")

### Location
fig_S3a <- ggplot(int_dat, aes(x = location, y = interaction_prop)) +
  geom_violin(fill = int_colour, alpha = 0.1, colour = "white") +
  geom_jitter(width = 0.07, colour = int_colour, size = 2, alpha = 0.8) +
  labs(x = "Location of operations", y = "Proportion of days with interactions", tag = "a)") +
  scale_y_continuous(breaks = seq(0,1, by = 0.2), limits = c(-0.01,1.1)) +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank())


fig_S3b <- ggplot(dmg_dat, aes(x = location, y = damage_prop)) +
  geom_violin(fill = dmg_colour, alpha = 0.1, colour = "white") +
  geom_jitter(width = 0.07, colour = dmg_colour, size = 2, alpha = 0.8) +
  labs(x = "Location of operations", y = "Proportion of catch lost", tag = "b)") +
  scale_y_continuous(breaks = seq(0,1, by = 0.2), limits = c(-0.01,1.1)) +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank())

ggsave(fig_S3a + fig_S3b, filename = "../../Figures/extraction_20230112/figS3.jpeg",
       width =25, height = 12.5, units = "cm", dpi = 1300)

##______________________________________________________________________________
#### 5. Summary stats ####

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

## saving operational interaction summary
op_interaction %>% 
  dplyr::select(`Study ID` = acc_no, Reference = study_lab, DOI = doi,
                `Observation number` = obs_no, Country = country,
                `Interaction proportion` = interaction_prop,
                `Interaction trials (days)` = interaction_trials,
                `Damage proportion` = damage_prop,
                `Damage trials (days)` = damage_trials) %>% 
  write_csv(file = "../../Figures/tableS1.csv")


