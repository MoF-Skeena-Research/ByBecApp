###setup for ByBECApp
library(shiny)
library(sf)
library(data.table)
library(shinyWidgets)
library(ggplot2)
library(ggiraph)
library(scales)
library(rhandsontable)
library(shinyalert)
library(RPostgreSQL)
library(shinyjs)
library(leafgl)
library(leaflet)
library(colourvalues)
library(shinythemes)

##connect to database
###Read in climate summary data


subzones_colours_ref <- fread("./inputs/WNA_v12_HexCols.csv")
setnames(subzones_colours_ref,c("BGC","Col"))

drv <- dbDriver("PostgreSQL")
sapply(dbListConnections(drv), dbDisconnect)
con <- dbConnect(drv, user = "postgres", password = "Kiriliny41", host = "68.183.199.104", 
                 port = 5432, dbname = "spp_feas")

##data for edatopic grid
grd1x <- seq(1.5,4.5,1)
grd1y <- seq(1.5,7.5,1)
rects <- data.table(xmin = rep(c(0.5,2.5,3.5), each = 5),
                    xmax = rep(c(2.5,3.5,5.5), each = 5),
                    ymin = rep(c(0.5,1.5,3.5,5.5,7.5),3),
                    ymax = rep(c(1.5,3.5,5.5,7.5,8.5),3))
ids <- 1:15
idDat <- expand.grid(SMR = 0:7, SNR = c("A","B","C","D","E"))
idDat <- as.data.table(idDat)
setorder(idDat,SMR,SNR)
idDat[,ID := c(5,5,10,15,15,4,4,9,14,14,4,4,9,14,14,3,3,8,13,13,3,3,8,13,13,2,2,7,12,12,2,2,7,12,12,1,1,6,11,11)]
idDat[,edatopic := paste0(SNR,SMR)]
edaMaxCol <- "#440154FF"
edaMinCol <- "#FDE725FF"
assCols <- data.table(ID = c(1,2,3,4,5,0), 
                      Col = c("#E20000","#FF7B00","#FFEC00","#91FB00","#1DB000","#631758"))
assID <- data.table(assessment = c("Fail","Poor","Fair","Good","Excellent","UN"),
                    ID = c(1,2,3,4,5,0))
fhCols <- data.frame(hazard = c("High","Moderate","Low"), 
                     Col = c("#D80000","#FFEF01","#0CC200"))

#grRamp2 <- colorRamp(c("#443e3dFF","#c0c0c0ff"),alpha = T) ##colour ramp for gray values
taxaCols <- c("#443e3d","#876114","#3d7075","#443e3d","#443e3d")
taxaFreqCols <- data.table(sppsplit = c(1,1,1,2,2,2,3,3,3),
                           Freq = c(rep(c("High","Moderate","Low"),3)),
                           Col = c("#443e3d","#78716f","#d4d4d4",
                                   "#876114","#bf994d","#f5ce7f",
                                   "#3d7075","#75c0c7","#bff0f5"))

##initial input table for adding offsite trial
trialInit <- data.table(
  spp = character(1),
  numplanted = numeric(1),
  seedlot = character(1),
  assessment = character(1))
##pest types
pestCat <- fread("./inputs/Pest_Types.csv")
##setup species picker
treelist <- fread("./inputs/Tree_List_2021.csv")
treelist <- treelist[NotUse != "x",.(TreeCode,EnglishName, Group)]
treelist[,TreeCode := paste0(TreeCode," - ", EnglishName)]
#treelist <- rbind(treelist,data.table(TreeCode = "None",Group = "Conifer_BC"))
sppList <- list()
for(nm in c("Conifer_BC","Broadleaf_BC","Conifer_Native","Broadleaf_Native")){
  temp <- treelist[Group == nm, TreeCode]
  sppList[[nm]] <- temp
}
allSppNames <- unlist(sppList)
allSppNames <- substr(allSppNames,1,2)
allSppNames <- unname(allSppNames)

minStart <- dbGetQuery(con,"select min(planted) from offsite")[1,1]
maxStart <- dbGetQuery(con,"select max(planted) from offsite")[1,1]

offsiteNames <- dbGetQuery(con,"select distinct plotid from offsite")[,1]
offsiteProj <- dbGetQuery(con,"select distinct project_id from offsite")[,1]
##max suitability colours
##BGC colours
zones <- sort(unique(gsub("[[:lower:]]|[[:digit:]]","", subzones_colours_ref$BGC)))
subzones <- unique(subzones_colours_ref$BGC)

subzTransparent <- copy(subzones_colours_ref)
subzTransparent[,Col := "#FFFFFF00"]
setnames(subzTransparent,c("bgc","Transparent"))

eda <- dbGetQuery(con,"select * from eda")
eda <- as.data.table(eda)
suitcols <- data.table(Suit = c(1,2,3),Col = c("#443e3dFF","#736e6eFF","#a29f9eFF"))#c("#42CF20FF","#ECCD22FF","#EC0E0EFF")
##climatic suitability colours
zonalOpt <- "#3e6837ff"
wetOpt <- data.table(feasible = c(1,2,3), Col = c("#c24f00ff","#cd804bff","#fbbd92ff"))
splitOpt <- "#df00a9ff"
dryOpt <- data.table(feasible = c(1,2,3), Col = c("#000aa3ff","#565edeff","#8b8fdbff"))

##legends
climaticLeg <- list(
  labels = c("Climatic Optimum","Wet Site Optimum","Dry Site Optimum","Bimodal Feasibility","Off-site Addition","Removed from CFRG"),
  colours = c(zonalOpt, wetOpt$Col[1], dryOpt$Col[1],splitOpt,"#fbff00ff","#8300ffff"),
  title = "Climatic Feasibility"
)

edaLeg <- list(
  labels = c("Poor Feasibility","Good Feasibility"),
  colours = c(edaMinCol,edaMaxCol),
  title = "Edatopic Feasibility"
)

instr <- tagList(
  p("To use this tool:"),
  tags$ol(
    tags$li("Select a species code to view its range and feasibility rating on the map."),
    tags$li("Select type of map: A) Presence/Absence, B) Climatic Suitability. C) Environmental suitability in select edatopic space 
                (click again to return to summarised view)."),
    tags$li("Click on a BGC polygon to display feasibility ratings in table format")),
  tags$hr(),
  p("Note: The values in the table can be updated and
                                           submitted to a database unless Climatic Suitability is selected. 
                                           To view updated feasibility toggle the Updated Feasibility Button"),
  p("There are several other options available:"),
  tags$ol(
    tags$li("By default the tool shows only BC. Toggle WNA to see rating across western north america"),
    tags$li("By default the tool does not show the BGC map. Shift slider to show colour-themed BGC"),
    tags$li("Show locations of actual tree species collections/observations in the dataset")),
  tags$hr()
)
