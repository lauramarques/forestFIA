#Build a SQLite database out of the FIA7 data you downloaded.
#All data downloaded on May 12, 2017 from https://apps.fs.usda.gov/fia/datamart/CSV/datamart_csv.html
#NOTE: this only needs to be done once!
#Alternatively you can update your DB this way if you replace these .csv files with new ones.
#first clear R envrionment, load packages.
#rm(list=ls())
#source('required_products_utilities/master_path_list.r')
library(RSQLite)

#get remote FIA data paths
FIA7_paths <- '~/data/FIA/FIA7_urls.txt'
FIA7_soils_paths <- '~/data/FIA/FIA7_soil_urls.txt'
FIA7.dir.path <- "~/data/FIA/FIA7/"

#Make the FIA7 directory.
mk_FIA_dir <- paste0('mkdir ',FIA7.dir.path)
system(paste0(mk_FIA_dir))
#download zip files to FIA7 directory. This takes a while.
get_FIA <- paste('wget -i',FIA7_paths,'-P',FIA7.dir.path)
system(paste0(get_FIA))
#unzip the files.
unzip_FIA <- paste0("unzip ",FIA7.dir.path,"'*.zip' -d ",FIA7.dir.path)
system(paste0(unzip_FIA))
#clean up and delete zip files.
clean_FIA <- paste0("rm ",FIA7.dir.path,'*.zip')
system(paste0(clean_FIA))

#specify file paths to forest data.
plot.path <- paste0(FIA7.dir.path,"PLOT.csv")
cond.path <- paste0(FIA7.dir.path,"COND.csv")
subplot.path <- paste0(FIA7.dir.path,"SUBPLOT.csv")
subp_cond.path <- paste0(FIA7.dir.path,"SUBP_COND.csv")
grm.path <- paste0(FIA7.dir.path,"TREE_GRM_ESTN.csv")
tree.path <- paste0(FIA7.dir.path,"TREE.csv")

#Create the database connection. This will also create the database if this is the first time you run the script.
#setwd to where you want to save new database the first time
setwd(FIA7.dir.path)
db <- dbConnect(SQLite(), dbname='FIA7.sqlite')

#add a table to the db
dbWriteTable(db, name="PLOT"         , value=     plot.path, row.names=FALSE, header=TRUE, sep = ",")
dbWriteTable(db, name="COND"         , value=     cond.path, row.names=FALSE, header=TRUE, sep = ",")
dbWriteTable(db, name="SUBPLOT"      , value=     subplot.path, row.names=FALSE, header=TRUE, sep = ",")
dbWriteTable(db, name="SUBP_COND"    , value=     subp_cond.path, row.names=FALSE, header=TRUE, sep = ",")
dbWriteTable(db, name="TREE_GRM_ESTN", value=     grm.path, row.names=FALSE, header=TRUE, sep = ",")
dbWriteTable(db, name="TREE"         , value=     tree.path, row.names=FALSE, header=TRUE, sep = ",")

