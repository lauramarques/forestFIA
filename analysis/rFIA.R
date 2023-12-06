devtools::install_github('hunter-stanke/rFIA')

library(rFIA)

# Downloading FIA data

## Download the state subset or Connecticut (requires an internet connection)
## Save as an object to automatically load the data into your current R session!
ct <- getFIA(states = 'CT', dir = "/Users/lauramarques/forestFIA_US/fia_data",tables = "COND",load = FALSE)
ct <- getFIA(states = 'CT', dir = "/Users/lauramarques/forestFIA_US/fia_data",tables = "PLOT")
ct <- getFIA(states = 'CT', dir = "/Users/lauramarques/forestFIA_US/fia_data",tables = "TREE")

az <- getFIA(states = 'AZ', dir = "/Users/lauramarques/forestFIA_US/fia_data",tables = "COND")
az <- getFIA(states = 'AZ', dir = "/Users/lauramarques/forestFIA_US/fia_data",tables = "PLOT")
az <- getFIA(states = 'AZ', dir = "/Users/lauramarques/forestFIA_US/fia_data",tables = "TREE")

str(ct$COND)
str(ct$PLOT)
str(ct$TREE)
length(unique(ct$COND$PLOT))
length(unique(ct$COND %>% filter(RESERVCD==1))$PLOT)
unique(ct$COND %>% filter(RESERVCD==1))$PLOT
unique(ct$COND %>% filter(RESERVCD==1))$INVYR

reserves <- ct$COND %>% filter(RESERVCD==1)


## Get multiple states worth of data (not saved since 'dir' is not specified)
northEast <- getFIA(states = c('ME', 'NH', 'VT', 'NY', 'CT', 'MA', 'RI'))

## Load FIA Data from a local directory
db <- readFIA("/Users/lauramarques/forestFIA_US/fia_data")
str(db$PLOT)

## Download data for PNW states, but don't load the data yet
getFIA(states = 'WA', 
       dir = "/Users/lauramarques/forestFIA_US/fia_data",
       load = FALSE)

## A simple call to readFIA will load and merge all states
allStates <- readFIA(dir = "/Users/lauramarques/forestFIA_US/fia_data")

## But using the 'states' argument we can select individual states (or groups)
wa <- readFIA(dir = "/Users/lauramarques/forestFIA_US/fia_data", states = 'WA')

## Read WA and OR, but not ID
wa_or <- readFIA(dir = "/Users/lauramarques/forestFIA_US/fia_data", states = c('WA', 'CT'))

# Estimate variables

## Load some data
data('fiaRI')
data('countiesRI')

## Most Recent Subset (2017)
riMR <- clipFIA(fiaRI) 
riMR$TREE

# More explicity (identical to above)
riMR <- clipFIA(fiaRI, mostRecent = TRUE)

## TPA & BAA for the most recent inventory year
tpaRI_MR <- tpa(riMR)

## All Inventory Years Available (i.e., returns a time series)
tpaRI <- tpa(fiaRI,totals = TRUE)

## Plot-level
tpaRI_plot <- tpa(riMR, byPlot = TRUE)

## Subplot-level
tpaRI_subp <- tpa(riMR, byPlot = TRUE, grpBy = SUBP)

## Tree-level
tpaRI_tree <- tpa(riMR, byPlot = TRUE, grpBy = TREE)

