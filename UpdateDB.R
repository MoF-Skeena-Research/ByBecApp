##script to add updates to database and merge changes
library(data.table)
library(pool)
library(RPostgres)

sppDb <- dbPool(
  drv = RPostgres::Postgres(),
  dbname = "spp_feas",
  host = "138.197.168.220",
  port = 5432,
  user = "postgres",
  password = "PowerOfBEC"
)

updates <- fread("Vancouver_Tree_Suitability_2021_Coast_7Oct2021.csv")
updates <- updates[,.(Region,SS_NoSpace,Spp,`Suitability(refguide)`,`COAST 5Feb+25Feb`,`COAST SEPT 2021`)]
setnames(updates,c("region","ss_nospace","spp","OrigFeas","Update1","Update2"))
##check that it's safe to combine the update columns
updates[!is.na(Update1) & !is.na(Update2),]
updates[!is.na(Update2),Update1 := Update2]
updates[,Update2 := NULL]
updates <- updates[!is.na(Update1),] ##only care about rows that actually have an update
updates[,OrigFeas := NULL]

##change to base codes to make sure it merges properly
updates[spp %in% c("Fdi","Fdc"),spp := "Fd"]
updates[spp %in% c("Pli","Plc"),spp := "Pl"]
updates[spp %in% c("Sw","Se","Sxw"),spp := "Sx"]
updates[spp %in% c("Ss", "Sxl","Sxs"),spp := "Ss"]
updates[spp %in% c("Pyi","Pyc"),spp := "Py"]
updates[spp %in% c("Acb","Act"),spp := "Ac"]

###now pull from database
dbFeas <- setDT(dbGetQuery(sppDb,"select * from feasorig"))
fwrite(dbFeas,"feasorig_save.csv")##just incase something goes wrong
feasNew <- merge(dbFeas,updates, by = c("ss_nospace","spp"), all = T)
##check that none of the updates conflict with previous updates
feasNew[!is.na(mod) & !is.na(Update1),]
##ok all good
reviewers <- "SAS-HAK"
feasNew[!is.na(Update1),`:=`(newfeas = Update1,mod = reviewers)]
feasNew[is.na(bgc),bgc := gsub("/.*","",ss_nospace)]
feasNew[is.na(sppsplit),temp := spp]
feasNew[temp == "Pl",temp := "Plc"]
feasNew[temp == "Fd",temp := "Fdc"]
feasNew[temp == "Ss",temp := "Sxs"]
feasNew[temp == "Sx",temp := "Sxw"]
feasNew[temp == "Ac",temp := "Act"]
feasNew[!is.na(temp), sppsplit := temp]
feasNew[,temp := NULL]

feasNew <- feasNew[,.(bgc,ss_nospace,sppsplit,feasible,spp,newfeas,mod)]
unique(feasNew$mod)
##warning! make sure you've saved a copy before dropping table###
dbExecute(sppDb,"drop table feasorig")
dbWriteTable(sppDb,"feasorig",feasNew,row.names = F)
dbGetQuery(sppDb,"select count(*) from feasorig")
dbExecute(sppDb,"create index on feasorig (bgc,sppsplit)")
dbExecute(sppDb,"create index on feasorig (spp)")
##update species names


