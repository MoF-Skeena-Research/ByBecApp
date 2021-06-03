library(data.table)
library(RPostgreSQL)
pest <- fread("Pest_High_Hazard.csv")
treeCodes <- fread("./inputs/Tree_List_2021.csv")
pest[treeCodes,TreeSpp := i.TreeCode,on = c(`HOST - SCIENTIFIC NAME` = "ScientificName")]
fwrite(pest,"TempPest.csv")
pest <- fread("TempPest.csv")
pest <- pest[,.(TreeSpp,`HOST - COMMON NAME`,CODE,`PEST - SCIENTIFIC NAME`,ZONE,SUBZONE,VARIANT)]
pest[,bgc := paste0(ZONE,SUBZONE,VARIANT)]
setnames(pest,c("treespp","tree_name","pest","pest_name","z","s","v","bgc"))
pest <- pest[,.(treespp,tree_name,pest,pest_name,bgc)]
pest[,treespp := substr(treespp,1,2)]
pest[,tree_name := tolower(tree_name)]
SppExpand <- data.table(tree_name = c(rep("spruce",3),rep("true fir",3)), 
                        expSpp = c("engelmann spruce","white spruce","sitka spruce",
                                   "douglas-fir","subalpine fir","amabalis fir"))
pest2 <- SppExpand[pest, on = "tree_name",allow.cartesian = T]
pest2[!is.na(expSpp),tree_name := expSpp]
pest2[tree_name == "amabilis fir", tree_name := "amabalis fir"]
BGCDat <- fread("All_BGCs_Info_v12_2.csv")
BGCDat <- BGCDat[DataSet == "BC",.(Zone,BGC)]
justZones <- pest2[grep("^[A-Z]+$",bgc),]
pest2 <- pest2[!grepl("^[A-Z]+$",bgc),]
justZones <- BGCDat[justZones, on = c(Zone = "bgc"),allow.cartesian = T]
justZones[,Zone := NULL]
setnames(justZones,old = "BGC", new = "bgc")
pest2 <- rbind(pest2,justZones)
pest2[,expSpp := NULL]
pest3 <- BGCDat[pest2, on = c(BGC = "bgc")]
pest3[,OldBGC := "Good"]
pest3[is.na(Zone), OldBGC := "Bad"]
pest3[,Zone := NULL]
pest3[,treespp := NULL]
pest3[tree_name == "amabalis fir", tree_name := "amabilis fir"]
treeCodes[,EnglishName := tolower(EnglishName)]
treeCodes <- treeCodes[,.(EnglishName,TreeCode)]
pest3 <- treeCodes[pest3, on = c(EnglishName = "tree_name")]
fwrite(pest3,"TempPest.csv") ##fix some species manually
pest3 <- fread("TempPest.csv")
pest3[,TreeCode := substr(TreeCode,1,2)]

BGCDat[,Subzone := gsub("[[:digit:]]","",BGC)]
badBGC <- pest3[OldBGC == "Bad",]
badBGC <- BGCDat[badBGC, on = c(Subzone = "BGC")]
badBGC[Subzone == "ESSFsc1",BGC := "ESSFdc1"]
badBGC[,Zone := NULL]
badBGC[,Subzone := NULL]
badBGC[,OldBGC := NULL]
goodBGC <- pest3[OldBGC != "Bad",]
goodBGC[,OldBGC := NULL]
pest3 <- rbind(goodBGC,badBGC)
pestBySpp <- unique(pest3[,.(TreeCode,pest)])


allSpp <- unique(pest2$treespp)
feas <- fread("~/CommonTables/Feasibility_v12_3.csv")
feas <- feas[Feasible %in% c(1,2,3),]
feas[,Spp := substr(SppVar, 1,2)]
feas <- feas[Spp %chin% allSpp,]
feas <- feas[BGC %chin% BGCDat$BGC,]
feas <- feas[,.(MF = max(Feasible)), by = .(Spp,SppVar,BGC)]
feas <- feas[,.(Spp,BGC)]
allOptions <- feas[pestBySpp,on = c(Spp = "TreeCode"),allow.cartesian = T]
allOptions[,ID := "UN"]
pest3[,Hazard := "High"]
allDat <- merge(pest3,allOptions, by.x = c("TreeCode","BGC","pest"), by.y = c("Spp","BGC","pest"), all = T)
allDat[is.na(Hazard),Hazard := "UN"]
allDat[,`:=`(ID = NULL, EnglishName = NULL, pest_name = NULL)]

pestName <- unique(pest3[,.(pest,pest_name)])
allDat[pestName, pest_name := i.pest_name, on = "pest"]
setnames(allDat,c("treecode","bgc","pest","hazard","pest_name"))
setcolorder(allDat,c("bgc","treecode","pest","pest_name","hazard"))
allDat[,hazard_update := hazard]
allDat[,mod := NA]
fwrite(allDat,"PestForDB.csv")

drv <- dbDriver("PostgreSQL")
sapply(dbListConnections(drv), dbDisconnect)
con <- dbConnect(drv, user = "postgres", password = "Kiriliny41", host = "68.183.199.104", 
                 port = 5432, dbname = "spp_feas")
dbExecute(con,"drop table forhealth")
dbWriteTable(con,"forhealth",allDat,row.names = F)
dbExecute(con,"create index on forhealth(bgc)")

orig <- fread("Pest_High_Hazard.csv")
orig <- orig[,1:6]
setnames(orig,c("pest","pest_common","pest_scientific","pest_code","host_common","host_scientific"))
pestNames <- unique(orig[,.(pest,pest_common,pest_scientific,pest_code)])
pest3 <- pestNames[pest3,on = c(pest_code = "pest")]
treeCodes <- fread("./inputs/Tree_List_2021.csv")
treeCodes <- treeCodes[,.(TreeCode,ScientificName)]
treeCodes[,TreeCode := substr(TreeCode,1,2)]
pest3[treeCodes,host_scientific := i.ScientificName, on = "TreeCode"]
pest3[,zone := gsub("[[:lower:]]|[[:digit:]]","",BGC)]
pest3[,subzone := gsub("[[:upper:]]|[[:digit:]]","",BGC)]
pest3[,variant := gsub("[[:alpha:]]","",BGC)]
setnames(pest3,old = c("EnglishName","TreeCode","BGC"),new = c("host_common","host_code","bgc"))
setcolorder(pest3,c("pest_code","pest","pest_common","pest_scientific","host_code","host_common","host_scientific",
                    "bgc","zone", "subzone","variant","Hazard"))
fwrite(pest3,"Pest_HighHazard_Clean.csv")
