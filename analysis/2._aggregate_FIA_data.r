#This script takes output of the FIA query and performs additional filtering and scaling.
#The output is 2 products:
#Product_1 - plot-level data, including plot-level recruitment.
#Product_2 - tree-level data, used for growth and mortality.
#time_series - Plot-level time series. Was used in prior analyses, not part of publication. Still here because it may become useful.
#This script requires a lot of memory, generally not possible on a laptop.
rm(list=ls())
library(data.table)
library(doParallel)
source('paths.r')
source('project_functions/tic_toc.r')

#register parallel environment.----
#Not necessary if I stop calculating conspecific basal area/density.
n.cores <- detectCores()
registerDoParallel(n.cores)

#load data from FIA queries.----
past3   <- readRDS(all.past3.path)
past2   <- readRDS(all.past2.path)
a.FIA.1 <- readRDS(all.past1.path)
a.FIA.2 <- readRDS(all.present.path)
FIA.states <- data.table(read.csv('required_products_utilities/FIA_state_codes_regions.csv'))
#put data frames in a list. This will make your life easier.
data.list <- list(a.FIA.1,a.FIA.2,past2,past3)
names(data.list) <- c('a.FIA.1','a.FIA.2','past2','past3')
#turn off scientific notation.
options(scipen = 999)
#load nodDB database.
nodDB <- data.table(read.csv(nodDB.path))
FIA.spcd <- read.csv('required_products_utilities/mycorrhizal_SPCD_data.csv')

#Remove quotes from CN values for both FIA data sets because they mess everything up.----
for(i in 1:length(data.list)){
  data.list[[i]]$PLT_CN      <- as.numeric(gsub('"', "",data.list[[i]]$PLT_CN     ))
  data.list[[i]]$PREV_PLT_CN <- as.numeric(gsub('"', "",data.list[[i]]$PREV_PLT_CN))
  data.list[[i]]$TRE_CN      <- as.numeric(gsub('"', "",data.list[[i]]$TRE_CN     ))
  data.list[[i]]$PREV_TRE_CN <- as.numeric(gsub('"', "",data.list[[i]]$PREV_TRE_CN))
}

#Testing?----
#Subset to one state to make sure code is working.
testing <- F
if(testing == T){
  #Subest to Tennessee. Has data for all 4 data subsets.
  for(i in 1:length(data.list)){
    data.list[[i]] <- data.list[[i]][data.list[[i]]$STATECD == 47,]
  }
}

#Assign N-fixation status.----
yes.nfix  <- c('Rhizobia','likely_Rhizobia','Frankia','Nostocaceae','likely_present','Present')
nodDB[Consensus.estimate %in% yes.nfix , nfix  := 1]
fix.genera <- as.character(nodDB[nodDB$nfix == 1,]$genus)
FIA.spcd$GENUS <- as.character(FIA.spcd$GENUS)
FIA.spcd$nfix <- ifelse(FIA.spcd$GENUS %in% fix.genera, 1, 0)
for(i in 1:length(data.list)){
  data.list[[i]] <- merge(data.list[[i]], FIA.spcd[,c('SPCD','nfix')], all.x = T)
}


#Remove any plots that have "clear evidence of artificial regeneration." STDORGCD == 1. -----
for(i in 1:length(data.list)){
  to.remove <- unique(data.list[[i]][STDORGCD == 1,]$PLT_CN)
  data.list[[i]] <- data.list[[i]][!(PLT_CN %in% to.remove),]
}

#Remove any plots that have a tree with STATUSCD = 3. These are plots where humans cut down a tree.----
for(i in 1:length(data.list)){
  to.remove <- unique(data.list[[i]][STATUSCD == 3,]$PLT_CN)
  data.list[[i]] <- data.list[[i]][!(PLT_CN %in% to.remove),]
}

#Remove plots where > 50% of trees died for any reason.----
#This removes plots where invasive pest outbreaks induced mass mortality.
for(i in 1:length(data.list)){
  test <- data.list[[i]]
  test$kill <- ifelse(test$AGENTCD > 0, 1, 0)
  sub <- data.frame(table(test$PLT_CN))
  colnames(sub) <- c('PLT_CN','stem.dens')
  kill <- data.frame(table(test[test$kill == 1,]$PLT_CN))
  colnames(kill) <- c('PLT_CN','kill')
  sub <- merge(sub, kill, by = 'PLT_CN',all.x = T)
  sub$kill <- ifelse(is.na(sub$kill), 0, sub$kill)
  sub$mort.rate <- sub$kill / sub$stem.dens
  test <- test[test$PLT_CN %in% sub[sub$mort.rate < 0.5,]$PLT_CN,]
  test$kill <- NULL
  data.list[[i]] <- test
}

#Remove all saplings (DIA < 5inches) based on microplot samplings.----
for(i in 1:length(data.list)){
  data.list[[i]] <- data.list[[i]][!(TPA_UNADJ == 74.965282),]
}

#Remove one random site that has very strange growth/recruitment numbers.----
#Fairly confident this is recovering from a recent clearcut, but was not indicated in other filters.
for(i in 1:length(data.list)){
  data.list[[i]] <- data.list[[i]][!(PLT_CN == 65355954010538),]
}

#Remove plots that don't have a previous plot number, a remper value of zero or a STDAGE of 0.----
for(i in 1:length(data.list)){
  data.list[[i]] <- data.list[[i]][!(is.na(PREV_PLT_CN)),]
  data.list[[i]] <- data.list[[i]][!(REMPER == 0),]
  #data.list[[i]] <- data.list[[i]][!(STDAGE == 0),] #These may have a stand age of zero, but thats clearly wrong since they have a REMPER value.
}
#Calculate number of species in each plot.----
for(i in 1:length(data.list)){
  data.list[[i]][, spp.count := uniqueN(SPCD), by = PLT_CN]
}
#Assign recruitment and ectomycorrhizal status at the individual level.----
for(i in 1:length(data.list)){
  data.list[[i]]$em           <- ifelse(data.list[[i]]$MYCO_ASSO == 'ECM', 1, 0)
  data.list[[i]]$recruit      <- ifelse(is.na(data.list[[i]]$PREV_TRE_CN), 1, 0)
  data.list[[i]]$recruit.em   <- data.list[[i]]$recruit * data.list[[i]]$em
  data.list[[i]]$recruit.am   <- data.list[[i]]$recruit * abs(data.list[[i]]$em - 1)
  data.list[[i]]$recruit.nfix <- data.list[[i]]$recruit * data.list[[i]]$nfix
  
}
#Flag trees that were living at beginning of interval for counting stem density later.----
for(i in 1:length(data.list)){
  data.list[[i]]$stem.live      <- ifelse(data.list[[i]]$recruit == 0                           , 1, 0)
  data.list[[i]]$stem.live.am   <- ifelse(data.list[[i]]$recruit == 0 & data.list[[i]]$em   == 0, 1, 0)
  data.list[[i]]$stem.live.em   <- ifelse(data.list[[i]]$recruit == 0 & data.list[[i]]$em   == 1, 1, 0)
  data.list[[i]]$stem.live.nfix <- ifelse(data.list[[i]]$recruit == 0 & data.list[[i]]$nfix == 1, 1, 0)
}

#Determine if a tree died over measurement interval at the individual level.----
#flag whether a tree died or not, for any reason. If it did, there will be a value associated with "AGENTCD" greater than 0.
for( i in 1:length(data.list)){
  data.list[[i]]$mortality <- ifelse(data.list[[i]]$AGENTCD > 0, 1, 0)
}

#Trees that are new (cross 5in threshold) during the remeasurement period don't count as surviving the measurement period. give them NA values.
for(i in 1:length(data.list)){
  data.list[[i]]$mortality <- ifelse(data.list[[i]]$PREV_TRE_CN > 0, data.list[[i]]$mortality, 0)
  #data.list[[i]][,mortality := ifelse(PREV_TRE_CN > 0, mortality, NA)]
}

#current and previous basal area in cm2.----
for(i in 1:length(data.list)){
  data.list[[i]]$DIA     <- as.numeric(data.list[[i]]$DIA    )
  data.list[[i]]$PREVDIA <- as.numeric(data.list[[i]]$PREVDIA)
  data.list[[i]][,BASAL    := pi*((2.54*DIA    )/2)^2]
  data.list[[i]][,PREVBASAL:= pi*((2.54*PREVDIA)/2)^2]
}

#diameter in centimeters.----
for(i in 1:length(data.list)){
  data.list[[i]][,    DIA.cm :=     DIA*2.54]
  data.list[[i]][,PREVDIA.cm := PREVDIA*2.54]
  data.list[[i]]$REMPER <- as.numeric(data.list[[i]]$REMPER)
  data.list[[i]][,inc.cm2.yr := (DIA.cm - PREVDIA.cm)/REMPER]
}

##################################################################
##### Product 1. Plot-level data, including recruitment.    ######
##################################################################
cat('Building plot-level product 1 and time series...\n')
#generate lists of myc types and PFTs.-----
 em.list <- levels(a.FIA.2$MYCO_ASSO)
pft.list <- levels(a.FIA.2$PFT)
all.list <- c(em.list,pft.list)

#convert some things to numeric that should be.
for(i in 1:length(data.list)){
  data.list[[i]][,PREVDIA := as.numeric(PREVDIA)]
  data.list[[i]][, REMPER := as.numeric(REMPER) ]
}

#if you are currently dead, your current basal area is assigned NA.----
for(i in 1:length(data.list)){
  data.list[[i]]$BASAL <- ifelse(data.list[[i]]$AGENTCD > 0, NA, data.list[[i]]$BASAL)
}

#calculate current basal area of all trees by mycorrhizal type, nfix and PFT.----
#myc type
for(i in 1:length(data.list)){
  for(k in 1:length(em.list)){
    name <- paste0('BASAL.',em.list[k])
    data.list[[i]][MYCO_ASSO == em.list[k],new := BASAL]
    setnames(data.list[[i]], 'new', name)
  }
}
#pft
for(i in 1:length(data.list)){
  for(k in 1:length(pft.list)){
    name <- paste0('BASAL.',pft.list[k])
    data.list[[i]][MYCO_ASSO == pft.list[k],new := BASAL]
    setnames(data.list[[i]], 'new', name)
  }
}
#nfix
for(i in 1:length(data.list)){
  data.list[[i]]$BASAL.nfix <- data.list[[i]]$BASAL * data.list[[i]]$nfix
}

#begin aggregation of tree-level data to plot level.----
scaled.list <- list()
for(i in 1:length(data.list)){
  if(nrow(data.list[[i]]) == 0){
    next
  }
  scaled <- aggregate(data.list[[i]]$BASAL ~ data.list[[i]]$PLT_CN, FUN = 'sum', na.rm = T, na.action = na.pass)
  names(scaled) <- c('PLT_CN','plot.BASAL')
  scaled.list[[i]] <- scaled
  names(scaled.list)[i] <- names(data.list)[i]
}

#aggregate basal area per plot by myctype and PFT.----
#add nfix to all.list
all.list <- c(all.list,'nfix')
for(i in 1:length(scaled.list)){
  for(k in 1:length(all.list)){
    name <- paste0('BASAL.',all.list[k])
    scaled.list[[i]]$new <- aggregate(data.list[[i]][[name]] ~ data.list[[i]]$PLT_CN, FUN='sum',na.rm=T, na.action=na.pass)[,2]
    setnames(scaled.list[[i]],'new',name)
  }
}

#Get plot level recruitment and stem density, broken out by AM, EM and nfix.----
for(i in 1:length(scaled.list)){
  #Pay special attention to cases where all trees in plot are new recruits.
  #recruitment.
  scaled.list[[i]]$recruit      <- aggregate(recruit      ~ PLT_CN, FUN = 'sum', data = data.list[[i]],na.rm=T,na.action=na.pass)[,2]
  scaled.list[[i]]$recruit.am   <- aggregate(recruit.am   ~ PLT_CN, FUN = 'sum', data = data.list[[i]],na.rm=T,na.action=na.pass)[,2]
  scaled.list[[i]]$recruit.em   <- aggregate(recruit.em   ~ PLT_CN, FUN = 'sum', data = data.list[[i]],na.rm=T,na.action=na.pass)[,2]
  scaled.list[[i]]$recruit.nfix <- aggregate(recruit.nfix ~ PLT_CN, FUN = 'sum', data = data.list[[i]],na.rm=T,na.action=na.pass)[,2]
  #stem density.
  scaled.list[[i]]$stem.density <- aggregate(stem.live      ~ PLT_CN, FUN = 'sum', data = data.list[[i]],na.rm=T,na.action=na.pass)[,2]
  scaled.list[[i]]$  em.density <- aggregate(stem.live.am   ~ PLT_CN, FUN = 'sum', data = data.list[[i]],na.rm=T,na.action=na.pass)[,2]
  scaled.list[[i]]$  am.density <- aggregate(stem.live.em   ~ PLT_CN, FUN = 'sum', data = data.list[[i]],na.rm=T,na.action=na.pass)[,2]
  scaled.list[[i]]$nfix.density <- aggregate(stem.live.nfix ~ PLT_CN, FUN = 'sum', data = data.list[[i]],na.rm=T,na.action=na.pass)[,2]
}

#pop in relevant data from plot table by taking medians.----
for(i in 1:length(scaled.list)){
  scaled.list[[i]]$latitude    <- aggregate(data.list[[i]]$LAT      ~ data.list[[i]]$PLT_CN, FUN='median',na.rm=T,na.action=na.pass)[,2]
  scaled.list[[i]]$longitude   <- aggregate(data.list[[i]]$LON      ~ data.list[[i]]$PLT_CN, FUN='median',na.rm=T,na.action=na.pass)[,2]
  scaled.list[[i]]$elevation   <- aggregate(data.list[[i]]$ELEV     ~ data.list[[i]]$PLT_CN, FUN='median',na.rm=T,na.action=na.pass)[,2]
  scaled.list[[i]]$INVYR       <- aggregate(data.list[[i]]$INVYR    ~ data.list[[i]]$PLT_CN, FUN='median',na.rm=T,na.action=na.pass)[,2]
  scaled.list[[i]]$STATECD     <- aggregate(data.list[[i]]$STATECD  ~ data.list[[i]]$PLT_CN, FUN='median',na.rm=T,na.action=na.pass)[,2]
  scaled.list[[i]]$COUNTYCD    <- aggregate(data.list[[i]]$COUNTYCD ~ data.list[[i]]$PLT_CN, FUN='median',na.rm=T,na.action=na.pass)[,2]
  scaled.list[[i]]$STDAGE      <- aggregate(data.list[[i]]$STDAGE   ~ data.list[[i]]$PLT_CN, FUN='median',na.rm=T,na.action=na.pass)[,2]
  scaled.list[[i]]$REMPER      <- aggregate(data.list[[i]]$REMPER   ~ data.list[[i]]$PLT_CN, FUN='median',na.rm=T,na.action=na.pass)[,2]
  scaled.list[[i]]$n.trees     <- data.list[[i]][, .N, by = PLT_CN][,2]
  scaled.list[[i]]$PREV_PLT_CN <- aggregate(data.list[[i]]$PREV_PLT_CN ~ data.list[[i]]$PLT_CN, FUN='unique', na.action = na.pass)[,2]
  #calculate relative abundance of EM trees at time of soil sampling and total EM + AM
  scaled.list[[i]]$relEM    <- scaled.list[[i]]$BASAL.ECM  / scaled.list[[i]]$plot.BASAL
  scaled.list[[i]]$relNfix  <- scaled.list[[i]]$BASAL.nfix / scaled.list[[i]]$plot.BASAL
  scaled.list[[i]]$relEM.AM <- (scaled.list[[i]]$BASAL.ECM + scaled.list[[i]]$BASAL.AM) / scaled.list[[i]]$plot.BASAL
}

#remove sites that are less than 90% AM+ECM trees.----
for(i in 1:length(scaled.list)){
  scaled.list[[i]] <- scaled.list[[i]][!(scaled.list[[i]]$relEM.AM < 0.9),]
}
#remove sites where initial stem density is 0.----
for(i in 1:length(scaled.list)){
  scaled.list[[i]] <- scaled.list[[i]][scaled.list[[i]]$stem.density > 0,]
}

#save output.----
#Product_1 is a plot-level forest data for most recent sampling.
Product_1 <- scaled.list[[2]]
Product_1 <- as.data.frame(Product_1)
Product_1 <- Product_1[!is.na(Product_1$PLT_CN),]
saveRDS(Product_1, file=Product_1.path, version = 2)


#time series relEM modeling
time_series <- list(scaled.list[['a.FIA.2']], 
                    scaled.list[['a.FIA.1']], 
                    scaled.list[['past2']], 
                    scaled.list[['past3']])
names(time_series) <- c('present','past1','past2','past3')
saveRDS(time_series,time_series_dat.path, version = 2)
cat('Plot level Product 1 and time series plot level data sets constructed.\n')

#######################################################################
##### Product 2. Individual tree-level Growth and Mortality data ######
#######################################################################
cat('Building individual level product 2...\n')

#Growth and Mortality is modeled at the individual tree level. Take most recent data.
Product_2 <- data.list[[2]]
scaled.list[['a.FIA.2']] <- data.table(scaled.list[['a.FIA.2']])
Product_2      <- merge(Product_2     ,scaled.list[['a.FIA.2']][,.(relEM,relEM.AM,relNfix,stem.density,em.density,am.density,nfix.density,plot.BASAL,PLT_CN)], by = 'PLT_CN') 

#save Product 2 output.----
saveRDS(Product_2, Product_2.path, version = 2)
cat('Finished constructing individual level Product 2.\n')

#end script.
