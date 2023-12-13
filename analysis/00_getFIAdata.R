# This script reads forest data from the FIA - COND, PLOT and TREE tables
# selects unmanaged plots and calculates the variables needed for further analyses

# Load packages
# devtools::install_github('hunter-stanke/rFIA')
library(rFIA)
library(dplyr)
library(tidyverse)
library(readr)

filn <- paste0(here::here(), "/data/inputs/data_FIA.RData")

if (!file.exists(filn)){
  # Downloading FIA data
  states <- read.csv(paste0(here::here(), "/data/inputs/states.csv"))
  
  # Download FIA data ------
  # for all states
  # the dataset needed: COND, PLOT, TREE
  st <- states$State.abbreviation
  
  # Data unavailable for:  DC, MH
  st <- st[-which(st %in% c('DC','MH') )]
  
  for(i in st){
    getFIA(states = i, dir = paste0(here::here(), "/data/obs"), tables = "COND", load = FALSE)
    getFIA(states = i, dir = paste0(here::here(), "/data/obs"), tables = "PLOT", load = FALSE)
    options(timeout=3600)
    getFIA(states = i, dir = paste0(here::here(), "/data/obs"), tables = "TREE", load = FALSE)
  }
  
  # Read data -------
  # UNITCD Survey unit code
  # STATECD State code
  # COUNTYCD County code
  # PLOT Plot number
  
  ## PLOT table ------
  # meta info for forest plots
  data_plot <- list.files(path = paste0(here::here(), "/data/obs"), pattern = "*_PLOT.csv") %>%
    purrr::map(read.csv) %>% 
    lapply(\(x) mutate(x, across(ECO_UNIT_PNW, as.character))) %>%
    bind_rows() %>%
    
    # Make a unique ID for each plot, irrespective of time
    mutate(plotID = paste(UNITCD, STATECD, COUNTYCD, PLOT, sep = '_'))
  
  ## COND table -----
  # used for filtering unmanaged forest plots (reserves)
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
    mutate(plotID = paste(UNITCD, STATECD, COUNTYCD, PLOT, sep = '_'))
  
  ## TREE table ------
  # Aggregate from tree to forest plot level
  # Note: R session crushes due to the big size of the files. So, we read the files and save only the summaries at plot level.
  
  # DRYBIO_AG - aboveground biomass (DRYBIO_AG) contained in each tree (in pounds, libs)
  # TPA_UNADJ - trees per acre each tree represents
  # DIA - DBH (inches) 
  # 1 lb = 0.453 kg
  # 1 acre = 0.405 ha
  # 1 inch = 2.54 cm
  # 1 sq inch = 0.00064516 sq meter
  # abg_biomass: from pounds per acre to kg per ha = *0.453/0.405
  # density: from indiv per acre to indiv per ha = */0.405
  # dbh: from inches per acre to cm per ha = *2.54/0.405
  # BA: from sq. inches per acre to sq. m per ha = *0.00064516/0.405
  
  states <- read.csv(paste0(here::here(), "/data/inputs/states.csv"))
  st <- states$State.abbreviation
  
  # Data unavailable for:  DC, MH
  st <- st[-which(st %in% c('DC','MH') )]
  data_stand <- data.frame()
  for(i in st){
    currentDF <- read.csv(paste0(here::here(), "/data/obs/", i, "_TREE.csv"))
    currentDF <- currentDF %>% 
      mutate(plotID = paste(UNITCD, STATECD, COUNTYCD, PLOT, sep = '_')) %>%
      
      # Filter trees with dbh > 12 cm o (4.724 in)
      filter(DIA>=4.724) %>%
      
      # Filter trees alive
      filter(STATUSCD==1) %>%
      
      group_by(plotID, INVYR) %>%
      summarize(abg_biomass_kg_ha = sum(DRYBIO_AG * TPA_UNADJ * 0.453/0.405, na.rm = TRUE),
                density_ind_ha = sum(TPA_UNADJ / 0.405 , na.rm = TRUE),
                mean_dbh = mean(DIA * 2.54, na.rm = TRUE),
                BA_m2_ha = sum((pi*DIA*DIA/4)*TPA_UNADJ * 0.00064516/0.405, na.rm = TRUE)) %>% 
      ungroup() %>%
      mutate(QMD = sqrt(BA_m2_ha/(0.0000785*density_ind_ha)),
             logDensity = log(density_ind_ha),
             logQMD = log(QMD)) 
    
    data_stand <- rbind(data_stand, currentDF)
  }
  
  # We want to filter the unmanaged plots. For that we select those plots classified as Reserves.
  # COND table RESERVCD==1 represents the reserves, where no interventions have been carried out.
  data_cond_sel <- data_cond %>% 
    select(plotID, UNITCD, STATECD, COUNTYCD, PLOT, INVYR, RESERVCD) %>% 
    distinct(plotID,INVYR, .keep_all = TRUE) %>% 
    arrange(plotID) 
  
  # apply filter retaining only unmanaged (reserves)
  data_cond_unm <- data_cond_sel %>% 
    filter(RESERVCD == 1) %>% 
    arrange(plotID) 
  
  data_plot_sel <- data_plot %>% 
    select(plotID, INVYR, LAT, LON, ELEV)
  
  # Join tables
  data_FIA <- data_cond_unm %>% 
    left_join(data_plot_sel) %>% 
    inner_join(data_stand) %>% 
    arrange(plotID) 
  
  # Remove entries with density = 0
  data_FIA <- data_FIA %>% 
    filter(density_ind_ha > 0)
  
  save(data_FIA, file = paste0(here::here(), "/data/inputs/data_FIA.RData"))
  
} else {
  load(paste0(here::here(), "/data/inputs/data_FIA.RData"))
}

# Select plots with minimum 3 census
data_FIA <- data_FIA %>%
  group_by(plotID) %>% 
  mutate(n_census=n()) %>% 
  ungroup() %>% 
  filter(n_census>=3) %>%
  mutate(dataset="FIA") %>% 
  arrange(plotID) 

data_FIA |> 
  mutate(abg_biomass_g_m2 = abg_biomass_kg_ha * 1e3 * 1e-4) |> 
  ggplot(aes(abg_biomass_g_m2, ..density..)) +
  geom_histogram()

rbeni::plot_map_simpl() +
  geom_point(aes(LON, LAT, color = abg_biomass_kg_ha), data = data_FIA)

ggplot() + 
  geom_point(data = data_FIA, 
             aes(x = logQMD, y = logDensity), 
             alpha=0.5, 
             size = 1.5, 
             col="black",
             inherit.aes = FALSE
             ) 

# Checks ----
# get number of individuals per acre
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
            density_ind_ha = sum(TPA_UNADJ / 0.405 , na.rm = TRUE),
            mean_dbh = mean(DIA * 2.54, na.rm = TRUE),
            BA_m2_ha = sum((pi*DIA*DIA/4)*TPA_UNADJ * 0.00064516/0.405, na.rm = TRUE)) %>% 
  ungroup() %>%
  mutate(QMD=sqrt(BA_m2_ha/(0.0000785*density_ind_ha)),
         logDensity=log(density_ind_ha),
         logQMD=log(QMD)) %>%
  arrange(pltID) 

