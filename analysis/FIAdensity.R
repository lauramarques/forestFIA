
# Load some packages
library(rFIA)
library(dplyr)

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
data_cond <- list.files(path = paste0(here::here(), "/data/obs"), pattern = "*_COND.csv") %>%
  purrr::map(read.csv) %>% 
  #lapply(read_csv) %>%
  lapply(\(x) mutate(x, across(HABTYPCD1, as.character))) %>%
  bind_rows() %>%
  # Make a unique ID for each plot, irrespective of time
  mutate(pltID = paste(UNITCD, STATECD, COUNTYCD, PLOT, sep = '_'))
unique(data_cond$pltID)
data_plot <- list.files(path = paste0(here::here(), "/data/obs"), pattern = "*_PLOT.csv") %>%
  purrr::map(read.csv) %>% 
  lapply(\(x) mutate(x, across(ECO_UNIT_PNW, as.character))) %>%
  bind_rows() %>%
  # Make a unique ID for each plot, irrespective of time
  mutate(pltID = paste(UNITCD, STATECD, COUNTYCD, PLOT, sep = '_'))
unique(data_plot$pltID)
data_tree <- list.files(path = paste0(here::here(), "/data/obs"), pattern = "*_TREE.csv") %>%
  purrr::map(read.csv) %>% 
  bind_rows() %>%
  # Make a unique ID for each plot, irrespective of time
  mutate(pltID = paste(UNITCD, STATECD, COUNTYCD, PLOT, sep = '_'))
unique(data_tree$pltID)

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
data_tree <- data_tree %>% filter(PLOT %in% reserves$PLOT) 
length(unique(data_tree$PLOT))

## All Inventory Years Available (i.e., returns a time series)
tpaRI <- tpa(fiaRI,totals = TRUE)
## Plot-level
tpaRI_plot <- tpa(fiaRI, byPlot = TRUE, treeType = 'all') %>%
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
  summarize(density_ind_ha = sum(TPA_UNADJ , na.rm = TRUE)) %>%
  arrange(pltID) 



## Tree-level data from NFI
# DRYBIO_AG - aboveground biomass (DRYBIO_AG) contained in each tree (libs)
# TPA_UNADJ - trees per acre each tree represents
# DIA - DBH (inches) 
# 1 lb = 0.453 kg
# 1 acre = 0.405 ha
# 1 inch = 2.54 cm

fiaRI$TREE
str(fiaRI$TREE)
length(unique(fiaRI$TREE$PLOT)) 
length(unique(fiaRI$TREE$INVYR)) 

fiaRI$TREE %>%
  mutate(pltID = paste(UNITCD, STATECD, COUNTYCD, PLOT, sep = '_')) %>%
  # Filter trees with dbh > 12 cm o (4.724 in) STATUSCD==1
  #filter(DIA>=4.724) %>%
  # Filter trees alive
  filter(STATUSCD==1) %>%
  group_by(pltID,INVYR) %>%
  summarize(abg_biomass_kg_ha = sum(DRYBIO_AG * TPA_UNADJ * 0.453/0.405, na.rm = TRUE),
            density_ind_ha = sum(TPA_UNADJ * 0.405 , na.rm = TRUE),
            mean_dbh = mean(DIA * 2.54, na.rm = TRUE),
            mean_dbh2 = mean(DIA * TPA_UNADJ * 2.54/0.405, na.rm = TRUE))


summary(fiaRI$TREE$DIA* 2.54 *fiaRI$TREE$TPA_UNADJ/0.405)




den <- fiaRI$TREE %>%
  mutate(pltID = paste(UNITCD, STATECD, COUNTYCD, PLOT, sep = '_')) %>%
  group_by(pltID,INVYR) %>%
  summarize(density_ind_ha = sum(TPA_UNADJ , na.rm = TRUE)) %>%
  arrange(pltID) 




