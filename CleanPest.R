library(data.table)
library(RPostgreSQL)
pest <- fread("Mod_Pest_Orig.csv")
pest <- pest[,.(`HOST - COMMON NAME`,`PEST CODE`, `PEST - SCIENTIFIC NAME`,ZONE,SUBZONE,VARIANT)]
pest[,VARIANT := as.character(VARIANT)]
pest[is.na(VARIANT),VARIANT := ""]
pest[,bgc := paste0(ZONE,SUBZONE,VARIANT)]
setnames(pest,c("tree_name","pest","pest_name","z","s","v","bgc"))
pest <- pest[,.(tree_name,pest,pest_name,bgc)]
pest[,tree_name := tolower(tree_name)]
SppExpand <- data.table(tree_name = c(rep("true fir",3)), 
                        expSpp = c("douglas-fir","subalpine fir","amabilis fir"))
pest2 <- SppExpand[pest, on = "tree_name",allow.cartesian = T]
pest2[!is.na(expSpp),tree_name := expSpp]

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

BGCDat[,Subzone := gsub("[[:digit:]]","",BGC)]
badBGC <- pest3[OldBGC == "Bad",]
badBGC <- BGCDat[badBGC, on = c(Subzone = "BGC"),allow.cartesian = T]
unique(badBGC[is.na(BGC),Subzone])

bgcCrosswalk <- data.table(OldBGC = c("BWBSdk1","BWBSdk2","BWBSmw2","PPdh","PPdh1","ESSFwc1","ICHdk3",rep("IDFmk",5),"IDFdc1","IDFdc2",
                                      "MSmk","ICHdm1","ICHdm2","ICHdk4"),
                           NewBGC = c("BWBSdk","BWBSmk","BWBSmk","IDFxx2","IDFxx2","ESSFwh1","IDFdk3","ICHmk1","ICHmk2",
                                      "ICHmk3","ICHmk4","ICHmk5","IDFdc","IDFdc","MSdk","ICHdm","ICHdm","ICHdk"))

badBGC <- bgcCrosswalk[badBGC, on = c(OldBGC = "Subzone"),allow.cartesian = T]
badBGC[!is.na(NewBGC),BGC := NewBGC]
badBGC[,Zone := NULL]
badBGC[,NewBGC := NULL]
badBGC[,i.OldBGC := NULL]
badBGC[,OldBGC := NULL]
goodBGC <- pest3[OldBGC != "Bad",]
goodBGC[,OldBGC := NULL]
pest3 <- rbind(goodBGC,badBGC)

treeCodes <- fread("Tree_List_Working.csv")
treeCodes[,EnglishName := tolower(EnglishName)]
treeCodes <- treeCodes[,.(EnglishName,TreeCode)]
pest3 <- treeCodes[pest3, on = c(EnglishName = "tree_name")]

pest3[,TreeCode := substr(TreeCode,1,2)]
fwrite(pest3,"ModPest_Clean.csv")

##join
highPest <- dbGetQuery(con, "select * from forhealth")
highPest <- as.data.table(highPest)
highPest <- highPest[hazard != "UN",]
highPest[,hazard_update := NULL]
highPest[,mod := NULL]

modPest <- pest3
modPest[,hazard := "Moderate"]
modPest[,EnglishName := NULL]
setnames(modPest,c("treecode","bgc","pest","pest_name","hazard"))
lowPest <- fread("LowPest_Clean.csv")
lowPest[,hazard := "Low"]
lowPest[,EnglishName := NULL]
setnames(lowPest,c("treecode","bgc","pest","pest_name","hazard"))
allPest <- rbind(lowPest,modPest,highPest)
allPest <- unique(allPest)
test <- allPest[,.(Num = .N), by = .(bgc,pest,treecode)]
hazardRating <- data.table(hazard = c("Low","Moderate","High"),
                           rating = c(1,2,3))
allPest[hazardRating, rating := i.rating, on = "hazard"]
allPest <- allPest[,.(pest_name = pest_name[1],rating = max(rating)), by = .(treecode,bgc,pest)]
allPest[hazardRating, hazard := i.hazard, on = "rating"]
allPest[,rating := NULL]
fwrite(allPest,"Pest_Combined_Clean.csv")
pestBySpp <- unique(allPest[,.(treecode,pest)])


allSpp <- unique(allPest$treecode)
feas <- fread("~/CommonTables/Feasibility_v12_3.csv")
feas <- feas[Feasible %in% c(1,2,3),]
feas[,Spp := substr(SppVar, 1,2)]
feas <- feas[Spp %chin% allSpp,]
feas <- feas[BGC %chin% BGCDat$BGC,]
feas <- feas[,.(MF = max(Feasible)), by = .(Spp,SppVar,BGC)]
feas <- feas[,.(Spp,BGC)]
allOptions <- feas[pestBySpp,on = c(Spp = "treecode"),allow.cartesian = T]
allOptions[,ID := "UN"]
allDat <- merge(allPest,allOptions, by.x = c("treecode","bgc","pest"), by.y = c("Spp","BGC","pest"), all = T)
allDat[is.na(hazard),hazard := "UN"]
allDat[,`:=`(ID = NULL, EnglishName = NULL, pest_name = NULL)]
allDat <- na.omit(allDat)

pestName <- unique(allPest[,.(pest,pest_name)])
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
highPest <- dbGetQuery(con, "select * from forhealth")
highPest <- as.data.table(highPest)
highPest <- highPest[hazard != "UN",]
highPest[,hazard_update := NULL]
highPest[,mod := NULL]

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
