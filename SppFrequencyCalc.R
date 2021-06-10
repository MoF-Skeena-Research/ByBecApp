QRY <- paste0("select bgc,ss_nospace,sppsplit,spp,feasible 
              from feasorig where spp = 'Fd' and feasible in (1,2,3,4,5)")
feas <- as.data.table(dbGetQuery(con, QRY))
minDist <- feas[,.SD[feasible == min(feasible, na.rm = T)],by = .(bgc,sppsplit)]
tf2 <- minDist[feasible %in% c(4,5),]
minDist <- minDist[feasible %in% c(1,2,3),]
abUnits <- minDist[grep("[[:alpha:]] */[[:alpha:]]+$",ss_nospace),]
noAb <- minDist[!grepl("[[:alpha:]] */[[:alpha:]]+$",ss_nospace),]
abUnits <- eda[abUnits, on = "ss_nospace"] ##merge
abUnits <- abUnits[,.(Temp = if(any(grepl("C4",edatopic))) paste0(ss_nospace,"_01") else ss_nospace, feasible = feasible[1]),
                   by = .(bgc,ss_nospace,sppsplit,spp)]
abUnits[,ss_nospace := NULL]
setnames(abUnits,old = "Temp",new = "ss_nospace")
minDist <- rbind(noAb,abUnits)
minDist[,ID := if(any(grepl("01", ss_nospace)) & feasible[1] == 1) T else F, by = .(bgc,sppsplit)]
minDist[,Freq := NA_character_]
minDist[(ID),Freq := "High"]

minDist2 <- minDist[ID == F,]
minDist2[,ID := if(any(grepl("01", ss_nospace))) T else F, by = .(bgc,sppsplit)]
minDist2[(ID),Freq := "Moderate"]

minDist3 <- minDist2[ID == F,]
minEda <- eda[minDist3, on = "ss_nospace"]
minEda <- minEda[,.(AvgEda = mean(smr)), by = .(bgc,sppsplit,ss_nospace,feasible)]
minEda[,CentEda := abs(AvgEda - 3.5)]
minEda <- minEda[,.SD[CentEda == min(CentEda, na.rm = T)], by = .(bgc,sppsplit)]
lookupTab <- data.table(AvgEda = c(0,2,5,7),Freq = c("Low","Moderate","Low","Low"))
temp <- lookupTab[minEda, on = "AvgEda", roll = T]

t1 <- minDist[!is.na(Freq),.(Freq = Freq[1]), by = .(bgc,sppsplit)]
t2 <- minDist2[!is.na(Freq),.(Freq = Freq[1]), by = .(bgc,sppsplit)]
t3 <- temp[,.(Freq = Freq[1]), by = .(bgc,sppsplit)]
allFreq <- rbind(t1,t2,t3)

if(nrow(tf2) > 0){
  tf2[feasible == 4,Freq := "Added"]
  tf2[feasible == 5,Freq := "Removed"]
  tf2 <- tf2[,.(Freq = Freq[1]), by = .(bgc,sppsplit)]
  allFreq <- rbind(allFreq, tf2)
}
