##################################################################
##################################################################
##                                        
##        EXAMPLE PLOT-LEVEL ESTIMATATION PROCEDURES
##            IMPLEMENTED BY THE rFIA PACKAGE
##
##       Case study: Estimating biomass & carbon stocks 
##
##                     Hunter Stanke
##                    15 October 2019
##
##################################################################
##################################################################

## In the code below, we highlight the basic estimation procedures
## used to compute plot-level estimates of forest attributes from 
## Forest Inventory and Analysis (FIA) data. We will demonstrate 
## these procedures for two plots contained in the 'fiaRI' 
## dataset (included in the rFIA package) so that you can follow
## along with a small dataset.


## NOTE: The source code for rFIA will vary slightly from that 
## presented below as we designed rFIA to be as flexible and 
## computationally efficient as possible. Despite the differences
## in syntax and structure, the estimation procedures presented
## below are identical to those in rFIA. You can find and 
## download the full source code for rFIA from our GitHub repo:
## https://github.com/hunter-stanke/rFIA



# Load some packages
library(rFIA)
library(dplyr)

# Load the fiaRI dataset (included in rFIA)
data(fiaRI)


##################  TWO EXAMPLE PLOTS  ##########################
## Some unique identifiers for two plots in fiaRI database object
## For example use below, both measured in 2014
a <- 168263223020004  # One forested condition
b <- 168263219020004  # Two forested, one non-forested condition

## To compute estimates of tree biomass/carbon at the plot-level,
## we really only need the tree, condition, and plot tables. 
## In the lines below, we will produce subsets the rows of these 
## tables which pertain to our two plots of interest (a & b above)
# Subset the PLOT, COND, and TREE tables for plot A
plot_a <- filter(fiaRI$PLOT, CN == a)
plot_a <- filter(fiaRI$PLOT, PLOT == 288)

cond_a <- filter(fiaRI$COND, PLT_CN == a)
cond_a <- filter(fiaRI$COND, PLOT == 288)

tree_a <- filter(fiaRI$TREE, PLT_CN == a)
tree_a <- filter(fiaRI$TREE, PLOT == 288)

# Subset the PLOT, COND, and TREE tables for plot B
plot_b <- filter(fiaRI$PLOT, CN == b)
cond_b <- filter(fiaRI$COND, PLT_CN == b)
tree_b <- filter(fiaRI$TREE, PLT_CN == b)

## Now that we have the tables we need for our plots of interest,
## let's take a look at their COND tables and see how these plots 
## are different.
## NOTE: The COND table lists the conditions, or land classes
## present on an FIA plot. Conditions may be obvious, such as 
## when a plot intersects a forest edge (the forested area and 
## non-forested area would be separate conditions). Although,
## more subtle differences between forested area on a plot, 
## such as differences in reserved status, owner group, forest
## type, stand-size class, regeneration status, and stand density 
## can further define conditions.

## COND_STATUS_CD indicates the basic land classification of a 
## condition, whether it is forested, non-forest, water, etc...
## COND_STATUS_CD = 1 indicates forested
# Plot A
cond_a$COND_STATUS_CD ## One, forested condition
# Plot B
cond_b$COND_STATUS_CD ## Two forested, and one non-forest

## Since there are two forested conditions on PLOT B, what 
## is the basis for the distinction?
# FORTYPCD indicates the forest type of the condition
# PHYSCLCD indicates the Physiographic class (e.g. mesic 
# moist slope)
cond_b$FORTYPCD
cond_b$PHYSCLCD
## Looks like we have one forested condition in the Northern 
## red oak forest type (FORTYPCD = 505) occuring on mesic 
## flatwoods (PHYSCLCD = 21), and a second forested condition
## in the Red maple/lowland forest type (FORTYPCD = 708) on a 
## hydric swamp/bog (PHYSCLCD = 31). The NA values relate to 
## the non-forested condition on the plot (COND_STATUS_CD != 1)
## Hence it appears Plot B straddles an upland, wetland, and 
## non-forested boundary!




##############  BASIC ESTIMATION PROCEDURES  #####################
## Now that we have the data for our two example plots, let's 
## put them to work estimating tree biomass and carbon

## First, we will join the plot, condition, and tree tables for 
## each plot.
# Plot A
tbl_a <- plot_a %>%
  # Rename the CN column in plot, PLT_CN for simple joining
  mutate(PLT_CN = CN) %>%
  # Join tables
  left_join(cond_a, by = 'PLT_CN') %>%
  left_join(tree_a, by = c('PLT_CN', 'CONDID'))
# Plot B
tbl_b <- plot_b %>%
  # Rename the CN column in plot, PLT_CN for simple joining
  mutate(PLT_CN = CN) %>%
  # Join tables
  left_join(cond_b, by = 'PLT_CN') %>%
  left_join(tree_b, by = c('PLT_CN', 'CONDID'))

## NOTE: Joining tables is important when we want to produce 
## estimates grouped by fields in the plot or condition table.
## Otherwise, it would be possible to only use the TREE table 
## to compute estimates below.

## To produce an estimate of the aboveground biomass and carbon 
## per acre represented by all trees on the plot, we can simply
## take a sum the aboveground biomass and carbon (CARBON_AG) 
## contained in each tree (DRYBIO_AG) multiplied by the trees 
## per acre each tree represents (TPA_UNADJ)
# Plot A
all_a <- tbl_a %>%
  group_by(PLT_CN) %>%
  summarize(BIO_AG_ACRE = sum(DRYBIO_AG * TPA_UNADJ / 2000, na.rm = TRUE),
            CARB_AG_ACRE = sum(CARBON_AG * TPA_UNADJ / 2000, na.rm = TRUE))

tree_a %>%   
  group_by(PLOT,INVYR) %>%
  summarize(BIO_AG_ACRE = sum(DRYBIO_AG * TPA_UNADJ / 2000, na.rm = TRUE),
            CARB_AG_ACRE = sum(CARBON_AG * TPA_UNADJ / 2000, na.rm = TRUE))

# Plot B
all_b <- tbl_b %>%
  group_by(PLT_CN) %>%
  summarize(BIO_AG_ACRE = sum(DRYBIO_AG * TPA_UNADJ / 2000, na.rm = TRUE),
            CARB_AG_ACRE = sum(CARBON_AG * TPA_UNADJ / 2000, na.rm = TRUE))

## NOTE: We divide by 2000 here to convert estimates from lbs/acre to 
## tons/acre

## Now let's check our estimates against rFIA --> We've got a match!
fiaRI$TREE
rFIA_all <- biomass(fiaRI, byPlot = TRUE, treeType = 'all')
# Plot A
filter(rFIA_all, PLT_CN == a)
all_a
# Plot B
filter(rFIA_all, PLT_CN == b)
all_b



##############  UNIQUE DOMAINS OF INTEREST  #####################
## But what if I want to produce estimates for a specific kind of 
## tree? Say Northern Red Oak (SPCD = 833) which is greater than 
## 12 inches DBH (DIA > 12). We accomplish this using what 
## Bechtold and Patterson (2005) call a 'domain indicator' (see 
## Eq. 4.1, pg. 47 of the publication). This is essentially just
## a vector which indicates whether a tree (or plot, condition, etc.)
## is within our domain of interest (red oak > 12"). 

## To construct the domain indicator, we just need a vector which 
## is the same length as our joined table, and takes a value of 1 
## if the stem is in the domain and 0 otherwise

# Plot A
tbl_a <- tbl_a %>%
  mutate(tDI = if_else(SPCD == 833 & DIA > 12, 1, 0))
## The domain indicator
tbl_a$tDI
## How many trees meet the criteria?
sum(tbl_a$tDI, na.rm = TRUE)

# Plot B
tbl_b <- tbl_b %>%
  mutate(tDI = if_else(SPCD == 833 & DIA > 12, 1, 0))
## The domain indicator
tbl_b$tDI
## How many trees meet the criteria?
sum(tbl_b$tDI, na.rm = TRUE)


## Now we can use our new domain indicator (vector of 0s and 1s, 
## 'tDI')  to produce estimates for any type of tree we specify! 
## By adding 'tDI' to the basic estimation procedures below, we 
## force any stem which is not in our domain of interest to take 
## a value of zero. Therefore, only stems which are within the 
## domain of interest contribute to the plot-level estimate.
# Plot A
ro12_a <- tbl_a %>%
  group_by(PLT_CN) %>%
  summarize(BIO_AG_ACRE = sum(DRYBIO_AG * TPA_UNADJ * tDI / 2000, na.rm = TRUE),
            CARB_AG_ACRE = sum(CARBON_AG * TPA_UNADJ * tDI/ 2000, na.rm = TRUE))
# Plot B
ro12_b <- tbl_b %>%
  group_by(PLT_CN) %>%
  summarize(BIO_AG_ACRE = sum(DRYBIO_AG * TPA_UNADJ * tDI / 2000, na.rm = TRUE),
            CARB_AG_ACRE = sum(CARBON_AG * TPA_UNADJ * tDI / 2000, na.rm = TRUE))

## Now let's check our estimates against rFIA --> We've got a match!
rFIA_ro12 <- biomass(fiaRI, byPlot = TRUE, treeType = 'all', treeDomain = SPCD == 833 & DIA > 12)
# Plot A
filter(rFIA_ro12, PLT_CN == a)
ro12_a
# Plot B
filter(rFIA_ro12, PLT_CN == b)
ro12_b




##############  PRODUCING GROUPED ESTIMATES  #####################
## What if I want to produce estimates grouped by some attribute
## contained in the FIA Database, like forest type?
## We can accomplish by simply adding the attribute you want to 
## group by to the 'group_by' call in the estimation procedures 
## above. This will then sum the biomass and carbon per acre of 
## stems occuring on each forest type seperately.
# Plot A
for_a <- tbl_a %>%
  ## Adding FORTYPCD here
  group_by(PLT_CN, FORTYPCD) %>%
  summarize(BIO_AG_ACRE = sum(DRYBIO_AG * TPA_UNADJ / 2000, na.rm = TRUE),
            CARB_AG_ACRE = sum(CARBON_AG * TPA_UNADJ/ 2000, na.rm = TRUE))
# Plot B
for_b <- tbl_b %>%
  ## Adding FORTYPCD here
  group_by(PLT_CN, FORTYPCD) %>%
  summarize(BIO_AG_ACRE = sum(DRYBIO_AG * TPA_UNADJ/ 2000, na.rm = TRUE),
            CARB_AG_ACRE = sum(CARBON_AG * TPA_UNADJ / 2000, na.rm = TRUE))

## Now let's check our estimates against rFIA --> We've got a match!
rFIA_for <- biomass(fiaRI, byPlot = TRUE, treeType = 'all', grpBy = FORTYPCD)
# Plot A
filter(rFIA_for, PLT_CN == a) 
for_a # One forest type here, so only one row in the output
# Plot B
filter(rFIA_for, PLT_CN == b)
for_b # Two forest types here, so two rows in output. NA
      # value is for the non-forested area








