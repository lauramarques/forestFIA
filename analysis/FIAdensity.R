# This script reads forest data from the FIA - COND, PLOT and TREE tables
# selects unmanaged plots and calculates the variables needed for further analyses

# Load packages
library(rFIA)
library(dplyr)
library(tidyverse)
library(readr)

# Downloading FIA data
states <- read.csv(paste0(here::here(), "/data/obs/states.csv"))

# download FIA data for all States ####
# the dataset needed: COND, PLOT, TREE
st <- states$State.abbreviation
# Data unavailable for:  DC, MH
st <- st[-which(st %in% c('DC','MH') )]
for(i in st){
  getFIA(states = i, dir = paste0(here::here(), "/data/obs"),tables = "COND",load = FALSE)
  getFIA(states = i, dir = paste0(here::here(), "/data/obs"),tables = "PLOT",load = FALSE)
  options(timeout=3600)
  getFIA(states = i, dir = paste0(here::here(), "/data/obs"),tables = "TREE",load = FALSE)
}

# read data  ####
# UNITCD Survey unit code
# STATECD State code
# COUNTYCD County code
# PLOT Plot number
setwd(paste0(here::here(), "/data/obs"))

# COND table
data_cond <- list.files(path = paste0(here::here(), "/data/obs"), pattern = "*_COND.csv") %>%
  purrr::map(read.csv) %>% 
  #lapply(read_csv) %>%
  lapply(\(x) mutate(x, across(HABTYPCD1, as.character))) %>%
  lapply(\(x) mutate(x, across(HABTYPCD2, as.character))) %>%
  lapply(\(x) mutate(x, across(HABTYPCD1_DESCR_PUB_CD, as.character))) %>%
  lapply(\(x) mutate(x, across(HABTYPCD2_DESCR_PUB_CD, as.character))) %>%
  lapply(\(x) mutate(x, across(HABTYPCD1_PUB_CD, as.character))) %>%
  lapply(\(x) mutate(x, across(HABTYPCD2_PUB_CD, as.character))) %>%
  bind_rows() %>%
  # Make a unique ID for each plot, irrespective of time
  mutate(pltID = paste(UNITCD, STATECD, COUNTYCD, PLOT, sep = '_'))
length(unique(data_cond$pltID))

# PLOT table
data_plot <- list.files(path = paste0(here::here(), "/data/obs"), pattern = "*_PLOT.csv") %>%
  purrr::map(read.csv) %>% 
  lapply(\(x) mutate(x, across(ECO_UNIT_PNW, as.character))) %>%
  bind_rows() %>%
  # Make a unique ID for each plot, irrespective of time
  mutate(pltID = paste(UNITCD, STATECD, COUNTYCD, PLOT, sep = '_'))
length(unique(data_plot$pltID))

# We want to filter the unmanaged plots. For that we select those plots classified as Reserves.
# COND table RESERVCD==1 represents the reserves, where no interventions have been carried out.
length(unique(data_cond$PLOT))
reserves <- data_cond %>% filter(RESERVCD==1)
length(unique(reserves$PLOT))

data_cond <- data_cond %>% filter(PLOT %in% reserves$PLOT)
length(unique(data_cond$PLOT))
str(data_cond)
data_plot <- data_plot %>% filter(PLOT %in% reserves$PLOT)
length(unique(data_plot$PLOT))
str(data_plot)

# TREE table.
# Note: R session crushes due to the big size of the files. So, we read the files and save only the reserves plots.
states <- read.csv(paste0(here::here(), "/data/obs/states.csv"))
st <- states$State.abbreviation
# Data unavailable for:  DC, MH
st <- st[-which(st %in% c('DC','MH') )]
st1 <- st[1:30]
data_tree <- data.frame()
for(i in st1){
  currentDF <- read.csv(paste0(here::here(), "/data/obs/", i, "_TREE.csv"))
  currentDF <- currentDF %>% filter(PLOT %in% reserves$PLOT) #%>% mutate(CAVITY_USE_PNWRS=as.character(CAVITY_USE_PNWRS)) 
  data_tree <- rbind(data_tree, currentDF)
  print(i)
}
length(unique(data_tree$PLOT))
str(data_tree)
write.csv(data_cond, paste0(here::here(), "/data/obs/data_cond.csv"))


st2 <- st[31:45]
data_tree2 <- data.frame()
for(i in st2){
  currentDF <- read.csv(paste0(here::here(), "/data/obs/", i, "_TREE.csv"))
  currentDF <- currentDF %>% filter(PLOT %in% reserves$PLOT) #%>% mutate(CAVITY_USE_PNWRS=as.character(CAVITY_USE_PNWRS)) 
  data_tree2 <- rbind(data_tree2, currentDF)
  print(i)
}
length(unique(data_tree2$PLOT))
str(data_tree2)


## Plot-level

tpa(fiaRI, byPlot = TRUE, treeType = 'all') %>%
  arrange(pltID) 

tpa(fiaRI, byPlot = TRUE, treeType = 'live', treeDomain = DIA >=4.724) %>%
  arrange(pltID)

str(fiaRI$TREE)

fiaRI$TREE %>% 
  # Filter trees with dbh > 12 cm o (4.724 in) STATUSCD==1
  filter(DIA>=4.724) %>%
  # Filter trees alive
  filter(STATUSCD==1) %>%
  mutate(pltID = paste(UNITCD, STATECD, COUNTYCD, PLOT, sep = '_')) %>%
  group_by(pltID,INVYR) %>%
  summarize(abg_biomass_kg_ha = sum(DRYBIO_AG * TPA_UNADJ * 0.453/0.405, na.rm = TRUE),
            density = sum(TPA_UNADJ , na.rm = TRUE),
            density_ind_ha = sum(TPA_UNADJ * 0.405 , na.rm = TRUE),
            mean_dbh = mean(DIA * 2.54, na.rm = TRUE),
            mean_dbh2 = mean(DIA * TPA_UNADJ * 2.54 * 0.405, na.rm = TRUE),
            BA_m2_ha = sum((pi*DIA*DIA/4)*TPA_UNADJ/144/4.356)) %>% # /144 to convert square inches to sq ft per acre
  ungroup() %>%
  mutate(QMD=sqrt(BA_m2_ha/(0.0000785*density_ind_ha)),
         logDensity=log(density_ind_ha),
         logQMD=log(QMD)) %>%
  arrange(pltID) 

## Tree-level data from NFI
# DRYBIO_AG - aboveground biomass (DRYBIO_AG) contained in each tree (libs)
# TPA_UNADJ - trees per acre each tree represents
# DIA - DBH (inches) 
# 1 lb = 0.453 kg
# 1 acre = 0.405 ha
# 1 inch = 2.54 cm
# 1 foot = 0.305 meters

fiaRI$TREE
str(fiaRI$TREE)
length(unique(fiaRI$TREE$PLOT)) 
length(unique(fiaRI$TREE$INVYR)) 

data_tree <- data_tree %>%
  mutate(pltID = paste(UNITCD, STATECD, COUNTYCD, PLOT, sep = '_')) %>%
  # Filter trees with dbh > 12 cm o (4.724 in) STATUSCD==1
  #filter(DIA>=4.724) %>%
  # Filter trees alive
  filter(STATUSCD==1) %>%
  group_by(pltID,INVYR) %>%
  summarize(abg_biomass_kg_ha = sum(DRYBIO_AG * TPA_UNADJ * 0.453/0.405, na.rm = TRUE),
            density_ind_ha = sum(TPA_UNADJ * 0.405 , na.rm = TRUE),
            mean_dbh = mean(DIA * TPA_UNADJ * 2.54 * 0.405, na.rm = TRUE),
            BA_m2_ha = sum((pi*DIA*DIA/4)*TPA_UNADJ/144/4.356)) %>% # /144 to convert square inches to sq ft per acre
  ungroup() %>%
  mutate(QMD=sqrt(BA_m2_ha/(0.0000785*density_ind_ha)),
         logDensity=log(density_ind_ha),
         logQMD=log(QMD)) 

# Select plots with minimum 3 census
data_tree_fil <- data_tree %>%
  group_by(pltID) %>% mutate(n_census=n()) %>% ungroup() %>% filter(n_census>=3)
length(unique(data_tree_fil$pltID))








