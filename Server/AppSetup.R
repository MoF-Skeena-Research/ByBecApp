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
mbtk="pk.eyJ1Ijoid2htYWNrZW4iLCJhIjoiY2twaDVkNXU5MmJieTJybGE3cWRtY3Q4aCJ9.ISBkzSHFfrr78AVP2y2FeQ"
mblbsty = "whmacken/ckph5q6d21q1318nz4shnyp20"
mbsty="whmacken/ckph5e7y01fhr17qk5nhnpo10"

subzones_colours_ref <- fread("./inputs/WNA_v12_HexCols.csv")
setnames(subzones_colours_ref,c("BGC","Col"))

drv <- dbDriver("PostgreSQL")
sapply(dbListConnections(drv), dbDisconnect)
con <- dbConnect(drv, user = "postgres", password = "postgres", host = "138.197.168.220", 
                 port = 5432, dbname = "spp_feas")
climcon <- dbConnect(drv, user = "postgres", password = "postgres", host = "138.197.168.220", 
                 port = 5432, dbname = "bgc_climate_data")

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
                           Col = c("#443e3d","#78716f","#828282",
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
treelist <- treelist[NotUse != "x",.(Sppsplit,TreeCode,EnglishName, Group)]
treelist[,TreeCode := paste0(TreeCode," - ", EnglishName)]
#treelist <- rbind(treelist,data.table(TreeCode = "None",Group = "Conifer_BC"))
sppList <- list()
sppSplitList <- list()
for(nm in c("Conifer_BC","Broadleaf_BC","Conifer_Native","Broadleaf_Native")){
  temp <- unique(treelist[Group == nm, TreeCode])
  sppList[[nm]] <- temp
  sppSplitList[[nm]] <- treelist[Group == nm, Sppsplit]
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
zones <- fread("inputs/BGCZones.csv")
zones <- zones$Zone
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

instr_feas <- tagList(
  h3("Tree Feasibility"),
  h4("Map Options"),
  tags$ol(
    tags$li("Select a species code to view its range and feasibility rating on the map."),
    tags$li("By default, the map shows original values. Check box to show updated values instead."),
    tags$li("By default, the map shows summarised range by BGC, shaded by frequency."),
    tags$li("To view feasibiliy ratings, select an edatopic location on the grid. 
            The map shading then represents mean feasibility. Click again to return to range view"),
    tags$li("To view actual locations of the selected species from plot data or offsite trials, select
            the respective radio buttons.")),
  tags$hr(),
  h4("View and Update Values"),
  tags$ol(
    tags$li("To view data in table form, click on a BGC"),
    tags$li("To update values, change data in the table, click submit, and add your initials."),
    tags$li("To add a species, click 'Add Species'; to remove a species, delete all entries in table.")
  ),
  p("Note: The button at the top right of the map allows you to select layers to show, and change
    the base layer. The slider on the bottom changes BGC layer opacity (zoom in or out for changes to
    take effect).")
)

instr_offsite <- tagList(
  h3("Off-site Trials"),
  h4("Map Options"),
  tags$ol(
    tags$li("Select one or more trial categories to display locations on the map"),
    tags$li("There are various filters: you can show plots that contain a certain species,
            have multiple species, or filter by date planted.")
    ),
  tags$hr(),
  h4("View and Update Plots"),
  p("There are two way to input data:"),
  tags$ol(
    tags$li("To add a new plot, use the form on the side panel - note you can click on the 
            map to select a location"),
    tags$li("To add or update an assessment on an existing trial, click on the plot (or select the
            id from the dropdown) and update values in the assessment column of the table below the map. 
            Don't forget to submit!")
  ),
  p("Note: The button at the top right of the map allows you to select layers to show, and change
    the base layer. The slider on the bottom changes BGC layer opacity (zoom in or out for changes to
    take effect).")
)

instr_forhealth <- tagList(
  h3("Forest Health"),
  h4("Map Options"),
  p("The map shows the range of the selected host, and the hazard rating for the selected pest,
    if it exists."),
  tags$ol(
    tags$li("Select a host species"),
    tags$li("Select a pest from the dropdown (note that the dropdown only contains
            pests which effect the selected host).")
  ),
  tags$hr(),
  h4("View and Update Hazard"),
  p("There are two way to input data:"),
  tags$ol(
    tags$li("Once a host and pest is specified, the side panel table shows the data rating
           for all suitible BGCs. Update the 'hazard_update' column and submit."),
    tags$li("To update a selected BGC, click on the map. The table below the map displays
            a pest by host matrix which can also be updated. Greyed out cells represent non-existant
            combinations.")
  ),
  p("Note: The button at the top right of the map allows you to select layers to show, and change
    the base layer. The slider on the bottom changes BGC layer opacity (zoom in or out for changes to
    take effect).")
)
