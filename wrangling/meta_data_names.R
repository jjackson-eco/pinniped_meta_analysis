#####################################################
##                                                 ##
##   Pinniped Fisheries Interaction Meta-Analysis  ##
##                                                 ##
##              Wrangling Meta-Data                ##
##                                                 ##
##              JJ- April 21st 2022                ##
##                                                 ##
#####################################################
rm(list = ls())
options(width = 100)

library(tidyverse)
library(patchwork)

##______________________________________________________________________________
#### 1. Loading ####

### OLD
pinn_meta <- read_csv("../../Data/acc_no_meta_data_20220414.csv")

##______________________________________________________________________________
#### 2. Wrangling ####

study_names <- pinn_meta %>% 
  mutate(study = unlist(lapply(X = 1:nrow(pinn_meta), function(x){
  crow = pinn_meta[x,]
  
  # lead + second
  c_lead = unlist(str_split(crow$Authors,pattern = ","))[1] %>% 
    gsub(x = ., pattern = "[[:space:]].*", replacement = "")
  
  c_second = unlist(str_split(crow$Authors,pattern = ","))[2] %>%
    str_trim(., side = "left") %>% 
    gsub(x = ., pattern = "[[:space:]].*", replacement = "")
  
  # number of authors
  n_auth <- length(unlist(str_split(crow$Authors,pattern = ",")))
  
  # generate study based on number of authors
  if(n_auth == 1){study = paste0(c_lead, " ", crow$Year)}
  else if(n_auth == 2){study = paste0(c_lead, " and ", c_second, " ", crow$Year)}
  else{study = paste0(c_lead, " et al. ", crow$Year)}
  
  # return
  return(study)}))) %>% 
  dplyr::select(acc_no, study)

##______________________________________________________________________________
#### 3. Saving ####

save(study_names, file = "../../Data/study_names.RData")


