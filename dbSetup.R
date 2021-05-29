library(RPostgreSQL)
library(data.table)
library(sf)

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, user = "postgres", password = "Kiriliny41", host = "68.183.199.104", 
                 port = 5432, dbname = "spp_feas")

dbSafeNames = function(names) {
  names = gsub('[^a-z0-9]+','_',tolower(names))
  names = make.names(names, unique=TRUE, allow_=TRUE)
  names = gsub('.','_',names, fixed=TRUE)
  names
}

feas <- fread("./inputs/Feasibility_v12_1.csv")
feas <- feas[,.(BGC,SS_NoSpace,Spp,Feasible)]
setnames(feas, old = "Spp",new = "SppSplit")
feas[,Spp := SppSplit]
feas[SppSplit %in% c("Fdi","Fdc"),Spp := "Fd"]
feas[SppSplit %in% c("Pli","Plc"),Spp := "Pl"]
feas[SppSplit %in% c("Sw","Se","Sxw"),Spp := "Sx"]
feas[SppSplit %in% c("Ss", "Sxl","Sxs"),Spp := "Ss"]
feas[SppSplit %in% c("Pyi","Pyc"),Spp := "Py"]
feas[SppSplit %in% c("Acb","Act"),Spp := "Ac"]
setnames(feas,c("bgc","ss_nospace","sppsplit","feasible","spp"))
feas[,newfeas := feasible]
feas[,mod := NA]

eda <- fread("./inputs/Edatopic_v12_1.csv")
eda <- eda[is.na(Special),.(BGC,SS_NoSpace,Edatopic)]
eda[,SMR := as.numeric(gsub("[[:alpha:]]","", Edatopic))]
feas <- feas[ss_nospace %chin% eda$SS_NoSpace,]
setnames(eda,dbSafeNames(colnames(eda)))

dbExecute(con,"DROP TABLE feasorig")
dbWriteTable(con, name = "feasorig", value = feas,row.names = F)
dbExecute(con,"CREATE INDEX ON feasorig(bgc,sppsplit)")
dbExecute(con,"CREATE INDEX ON feasorig(spp)")

dbExecute(con, "DROP TABLE eda")
dbWriteTable(con, name = "eda", value = eda,row.names = F)
dbExecute(con,"CREATE INDEX ON eda(bgc)")
dbDisconnect(con)

treeLocs <- fread("./inputs/TreeSppLocations.csv")
treeLocs <- treeLocs[,.(Spp,Latitude,Longitude,`Plot Number`)]
setnames(treeLocs,c("spp","lat","long","plotnum"))
treesf <- st_as_sf(treeLocs,coords = c("long", "lat"), 
                   crs = 4326)
st_write(treesf,con,"plotdata")

###plot data
bcPlots <- fread("TreeSpp.csv")

treeCross <- unique(usPlots[TreeCode != "",.(Species,TreeCode)])
bcPlots[treeCross,TreeCode := i.TreeCode, on = "Species"]
bcPlots <- bcPlots[!is.na(TreeCode),]
bcPlots <- bcPlots[!is.na(Longitude) & !is.na(Latitude),]
bcPlots[,Longitude := Longitude * -1]
bcPlotsSf <- st_as_sf(bcPlots, coords = c("Longitude","Latitude"),crs = 4326)
bcPlotsSf <- bcPlotsSf[,c("TreeCode","PlotNumber")]
colnames(bcPlotsSf)[1:2] <- c("spp","plotnum")
st_write(bcPlotsSf,con,"plotdata")

##BC##
dat <- fread("BCTreeLocations.csv")
crosstab <- fread("SppCodes.csv")
dat[crosstab, TreeCode := i.TreeCode, on = c(Species = "Code")]  
dat <- dat[!is.na(TreeCode),]
dat <- dat[!is.na(Longitude) & !is.na(Latitude),]
dat <- dat[,.(TreeCode,PlotNumber,Longitude,Latitude)]
dat[,Region := "BC"]
#dat[,spp := substr(TreeCode,1,2)]

##US and AB
usPlots <- fread("USA_Tree_Plot_Locations.csv")
usPlots <- usPlots[,.(Species,PlotNumber,Latitude,Longitude)]
usLookup <- fread("US_SppCross.csv")
usPlots[usLookup, TreeCode := i.TreeCode, on = c(Species = "US code")]
usPlots[crosstab, TreeCode2 := i.TreeCode, on = c(Species = "Code")]
usPlots[is.na(TreeCode),TreeCode := TreeCode2]
usPlots <- usPlots[,.(TreeCode,PlotNumber,Latitude,Longitude)]
usPlots <- usPlots[!is.na(TreeCode),]
usPlots[,Region := "US"]

##AB
abPlots <- fread("AB_Tree_Plot_Locations.csv")
abPlots <- abPlots[,.(TreeCode,`FIELD PLOT NUMBER`,LATITUDE,LONGITUDE)]
setnames(abPlots, c("TreeCode","PlotNumber","Latitude","Longitude"))
abPlots[,Region := "AB"]

allDat <- rbind(dat,usPlots,abPlots)
allDat[,spp := substr(TreeCode,1,2)]
allDatSf <- st_as_sf(allDat, coords = c("Longitude","Latitude"),crs = 4326)
colnames(allDatSf)[1:4] <- c("sppsplit","plotnum","region","spp")  
st_write(allDatSf,con,"plotdata")

##Offsite
dat <- readRDS("offsite_Locations.rds")
datSf <- st_as_sf(dat,coords = c("X","Y"), crs = 3005)
datSf <- datSf[,c("OPENING_ID","Species","NUMBER_PLANTED","SEEDLOT_NUMBER", "ATU_COMPLETION_DATE")]
colnames(datSf)[1:5] <- c("plotid","spp","numplanted","seedlot", "planted")
datSf <- st_transform(datSf,4326)
datSf$planted <- as.Date(datSf$planted)
datSf$project_id <- "RESULTS"

dat2 <- fread("./Trials/AMAT seedlots X sites_1.csv")
dat2 <- dat2[,.(SLnum, Sitename,Sp,Year,Lat_S,Long_S)]
metaDat <- fread("./Trials/AMAT_Seedlot.csv")
metaDat <- metaDat[,.(SLnum,Sp,Seedlot_Num)]
dat2[metaDat, Seedlot := i.Seedlot_Num, on = c("SLnum","Sp")]
dat2[,SLnum := NULL]
dat2Sf <- st_as_sf(dat2,coords = c("Long_S","Lat_S"), crs = 4326)
colnames(dat2Sf)[1:4] <- c("plotid","spp","planted","seedlot")
dat2Sf$planted <- as.Date(paste0(dat2Sf$planted,"-01-01"))
dat2Sf$numplanted <- NA
dat2Sf$project_id <- "AMAT"
datAll <- rbind(datSf,dat2Sf)
dbExecute(con,"drop table offsite")
st_write(datAll,dsn = con, "offsite")
dbExecute(con,"create index on offsite(spp)")
dbExecute(con,"create index on offsite(project_id,spp,planted)")
d2 <- st_read(con, query = "select * from offsite where spp = 'Lw'")
