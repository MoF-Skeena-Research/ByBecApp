##script to add updates to database and merge changes
library(data.table)
library(pool)
library(RPostgres)
require(dplyr)

sppDb <- dbPool(
  drv = RPostgres::Postgres(),
  dbname = "spp_feas",
  host = "138.197.168.220",
  port = 5432,
  user = "postgres",
  password = "PowerOfBEC"
)

# updates <- fread("Vancouver_Tree_Suitability_2021_Coast_7Oct2021.csv")
# updates <- updates[,.(Region,SS_NoSpace,Spp,`Suitability(refguide)`,`COAST 5Feb+25Feb`,`COAST SEPT 2021`)]
# setnames(updates,c("region","ss_nospace","spp","OrigFeas","Update1","Update2"))

fh <- dbGetQuery(sppDb, "select * from forhealth")
fwrite(fh, "ForestHealthDownload.csv")
##check that it's safe to combine the update columns
# updates[!is.na(Update1) & !is.na(Update2),]
# updates[!is.na(Update2),Update1 := Update2]
# updates[,Update2 := NULL]
# updates <- updates[!is.na(Update1),] ##only care about rows that actually have an update
# updates[,OrigFeas := NULL]

##change to base codes to make sure it merges properly
# updates[spp %in% c("Fdi","Fdc"),spp := "Fd"]
# updates[spp %in% c("Pli","Plc"),spp := "Pl"]
# updates[spp %in% c("Sw","Se","Sxw"),spp := "Sx"]
# updates[spp %in% c("Ss", "Sxl","Sxs"),spp := "Ss"]
# updates[spp %in% c("Pyi","Pyc"),spp := "Py"]
# updates[spp %in% c("Acb","Act"),spp := "Ac"]

###now pull from database
dbFeas <- setDT(dbGetQuery(sppDb,"select * from feasorig"))
fwrite(dbFeas,"feasorig_save4.csv") ##just in case something goes wrong
 #updaterate <- fread("./inputs/Feasibility_v12_11.csv")
# dbFeasNew <- left_join(dbFeas, updaterate)
# fwrite(dbFeasNew,"feasorig_save2.csv")##just incase something goes wrong
# feasNew <- merge(dbFeas,updates, by = c("ss_nospace","spp"), all = T)
##____________LOAD Externally updated spread shett to merge back into database

feasNew <- fread("C:/Users/kirid/Downloads/FeasibilityUpdates_ByBEC.csv")###read back in an updated csv file
feasNew[,feasible := NULL]
setnames(feasNew, old = c("newfeas","mod"), new = c("nf_update","mod_update"))
setnames(feasNew, old = "spp",new = "sppsplit")

check_updates <- merge(dbFeas, feasNew, by = c("bgc","ss_nospace","sppsplit"), all = T)
temp <- check_updates[newfeas != nf_update,]
temp <- temp[mod != "",]
fixed_conflicts <- fread("Conflicting_feas.csv")
fixed_conflicts <- fixed_conflicts[,.(ss_nospace,sppsplit,use,mod_use)]
check_updates[newfeas != nf_update,`:=`(newfeas = nf_update, mod = mod_update)]
check_updates[fixed_conflicts, `:=`(fix_feas = i.use, fix_mod = i.mod_use),
              on = c("ss_nospace","sppsplit")]
check_updates[!is.na(fix_feas),`:=`(newfeas = fix_feas,mod = fix_mod)]
newDat <- check_updates[,.(bgc, ss_nospace, sppsplit, feasible, spp, newfeas, 
                             mod)]
newDat[mod == "", mod := NA]
##check that none of the updates conflict with previous updates
#feasNew[!is.na(mod) & !is.na(Update1),]
##ok all good
#reviewers <- "SAS-HAK"
#feasNew[!is.na(Update1),`:=`(newfeas = Update1,mod = reviewers)]
# feasNew[is.na(bgc),bgc := gsub("/.*","",ss_nospace)]
# feasNew[is.na(sppsplit),temp := spp]
# feasNew[temp == "Pl",temp := "Plc"]
# feasNew[temp == "Fd",temp := "Fdc"]
# feasNew[temp == "Ss",temp := "Sxs"]
# feasNew[temp == "Sx",temp := "Sxw"]
# feasNew[temp == "Ac",temp := "Act"]
# feasNew[!is.na(temp), sppsplit := temp]
# feasNew[,temp := NULL]

feasNew <- feasNew[,.(bgc,ss_nospace,sppsplit,feasible,spp,newfeas,mod)] ##make sure it's only got the rows required
unique(feasNew$mod)
##warning! make sure you've saved a copy before dropping table###
dbExecute(sppDb,"drop table feasorig")
dbWriteTable(sppDb,"feasorig",newDat,row.names = F)
dbGetQuery(sppDb,"select count(*) from feasorig")
dbExecute(sppDb,"create index on feasorig (bgc,sppsplit)")
dbExecute(sppDb,"create index on feasorig (spp)")
##update species names


ycUpdate <- fread("~/Downloads/USAplotswithYc.csv")
ycUpdate[,`:=`(sppsplit = "Yc",spp = "Yc",region = "US")]
library(sf)
yc <- st_as_sf(ycUpdate,coords = c("Longitude","Latitude"), crs = 4326)
colnames(yc)[1] <- "plotnum"
st_write(yc,sppDb,"plotdata",append = T, row.names = F)
