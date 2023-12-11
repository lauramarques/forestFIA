# Load some packages
library(rFIA)
library(dplyr)
library(tidyverse)
library(readr)

# Downloading FIA data
states <- read.csv(paste0(here::here(), "/data/obs/states.csv"))

# download the dataset needed: COND, PLOT, TREE for all States
st <- states$State.abbreviation
# Data unavailable for:  DC, MH
st <- st[-which(st %in% c('DC','MH') )]
for(i in st){
  getFIA(states = i, dir = paste0(here::here(), "/data/obs"),tables = "COND",load = FALSE)
  getFIA(states = i, dir = paste0(here::here(), "/data/obs"),tables = "PLOT",load = FALSE)
  options(timeout=3600)
  getFIA(states = i, dir = paste0(here::here(), "/data/obs"),tables = "TREE",load = FALSE)
}

# read data
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
unique(data_cond$pltID)
#write.csv(data_cond, paste0(here::here(), "/data/df/data_cond.csv"))

# PLOT table
data_plot <- list.files(path = paste0(here::here(), "/data/obs"), pattern = "*_PLOT.csv") %>%
  purrr::map(read.csv) %>% 
  lapply(\(x) mutate(x, across(ECO_UNIT_PNW, as.character))) %>%
  bind_rows() %>%
  # Make a unique ID for each plot, irrespective of time
  mutate(pltID = paste(UNITCD, STATECD, COUNTYCD, PLOT, sep = '_'))
unique(data_plot$pltID)

# We want to filter the unmanaged plots. For that we select those plots classified as reserves.
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
data_tree <- list.files(path = paste0(here::here(), "/data/obs"), pattern = "*_TREE.csv") %>%
  purrr::map(read.csv) %>% 
  lapply(\(x) mutate(x, across(CAVITY_USE_PNWRS, as.character))) %>%
  lapply(\(x) filter(x, PLOT %in% reserves$PLOT)) %>% # Filter for the unmanaged plots
  bind_rows() %>%
  # Make a unique ID for each plot, irrespective of time
  mutate(pltID = paste(UNITCD, STATECD, COUNTYCD, PLOT, sep = '_'))
unique(data_tree$pltID)

st <- states$State.abbreviation
# Data unavailable for:  DC, MH
st <- st[-which(st %in% c('DC','MH') )]
#xxx <- read.csv(paste0(here::here(), "/data/obs/WI_TREE.csv"))
st <- st[1:25]
data_tree <- data.frame()
for(i in st){
  currentDF <- read.csv(paste0(here::here(), "/data/obs/", i, "_TREE.csv"))
  currentDF <- currentDF %>% filter(PLOT %in% reserves$PLOT)
  data_tree <- rbind(data_tree, currentDF)
  print(i)
}

data_tree <- data_tree %>% filter(PLOT %in% reserves$PLOT) 
length(unique(data_tree$PLOT))

# To calculate aboveground biomass (lbs/acre) represented by all trees on the plot, 
# take a sum the aboveground biomass (DRYBIO_AG) contained in each tree (DRYBIO_AG) multiplied by 
# the trees per acre each tree represents (TPA_UNADJ). Multiply for 1.121 to convert from lbs/acre to kg/ha.
data_biomass <- data_tree %>%
  group_by(pltID,INVYR) %>%
  summarize(abg_biomass_kg_ha = sum(DRYBIO_AG * TPA_UNADJ * 1.121, na.rm = TRUE))
