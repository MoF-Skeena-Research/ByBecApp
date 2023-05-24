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
library(RPostgres)
library(shinyjs)
library(leafgl)
library(leaflet)
library(leaflet.extras)
library(leaflegend)
library(colourvalues)
library(shinythemes)
library(gridExtra)
library(pool)

##connect to database
###Read in climate summary data
mbtk=Sys.getenv("BCGOV_MAPBOX_TOKEN")
mblbsty = Sys.getenv("BCGOV_MAPBOX_LABELS_STYLE")
mbsty=Sys.getenv("BCGOV_MAPBOX_HILLSHADE_STYLE")

sppDb <- dbPool(
  drv = RPostgres::Postgres(),
  dbname = "spp_feas",
  host = Sys.getenv("BCGOV_HOST"),
  port = 5432, 
  user = Sys.getenv("BCGOV_USR"),
  password = Sys.getenv("BCGOV_PWD")
)

climDb <- dbPool(
  drv = RPostgres::Postgres(),
  dbname = "bgc_climate_data",
  host = Sys.getenv("BCGOV_HOST"),
  port = 5432, 
  user = Sys.getenv("BCGOV_USR"),
  password = Sys.getenv("BCGOV_PWD")
)

gomDb <- dbPool(
  drv = RPostgres::Postgres(),
  dbname = Sys.getenv("GOM_DB"),
  host = Sys.getenv("GOM_HOST"),
  port = Sys.getenv("GOM_PORT"), 
  user = "postgres",
  password = Sys.getenv("GOM_PWD")
)


onStop(function() {
  poolClose(climDb)
  poolClose(sppDb)
  poolClose(gomDb)
})

subzones_colours_ref <- fread("./inputs/WNA_v12_HexCols.csv")
setnames(subzones_colours_ref,c("BGC","Col"))

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

edaFreqCols <- data.table(FeasVal = rev(seq(4,1,by = -0.5)), 
                          Col = rev(c('#40004b','#40004b','#762a83','#9970ab','#5aae61','#1b7837','#00441b')))
assCols <- data.table(ID = c(1,2,3,4,5,0), 
                      Col = c("#C30000","#E79B1A","#E6F000","#B4FB29","#008D1A","#4f4f4f"))
assID <- data.table(assessment = c("Fail","Poor","Fair","Good","Excellent","UN"),
                    ID = c(1,2,3,4,5,0))
fhCols <- data.frame(hazard = c("High","Moderate","Low"), 
                     Col = c("#D80000","#FFEF01","#0CC200"))

proj_names <- dbGetQuery(sppDb, "select distinct trial_type from offsite_site")[,1]

#grRamp2 <- colorRamp(c("#443e3dFF","#c0c0c0ff"),alpha = T) ##colour ramp for gray values
taxaCols <- c("#443e3d","#876114","#3d7075","#443e3d","#443e3d")
taxaFreqCols <- data.table(sppsplit = c(1,1,1,2,2,2,3,3,3),
                           Freq = c(rep(c("3","2","1"),3)),
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

##offsite
sppData <- dbGetQuery(sppDb, "select distinct spp from offsite_planting")[,1]
gomData <- dbGetQuery(gomDb, "select distinct species from planting_info")[,1]
sppData <- c(sppData,gomData)
sppList2 <- sppList
for(i in 1:length(sppList2)){
  sppList2[[i]] <- sppList2[[i]][substr(sppList2[[i]],1,2) %in% substr(sppData,1,2)]
}

symbolGuide <- data.table(trial_type = c("GOM", "Operational","Other","Research"),
                          symbol = c("triangle","circle","diamond","plus"))

minStart <- dbGetQuery(sppDb,"select min(plantingyear) from offsite_site")[1,1]
#maxStart <- dbGetQuery(sppDb,"select max(plantingyear) from offsite_site")[1,1]
maxStart <- 2023

offsiteNames <- dbGetQuery(sppDb,"select distinct plotid from offsite")[,1]
offsiteProj <- dbGetQuery(sppDb,"select distinct project_id from offsite")[,1]
offsiteTrials <- dbGetQuery(sppDb, "select trial_id from offsite_site")[,1]
sitenames <- as.data.table(st_read(sppDb, query = "select * from offsite_site limit 1"))
plantingnames <- as.data.table(dbGetQuery(sppDb, "select * from offsite_planting limit 0"))
##max suitability colours
##BGC colours
zones <- fread("inputs/BGCZones.csv")
zonesBC <- zones[Region == "BC",Zone]
zones <- zones$Zone
subzones <- unique(subzones_colours_ref$BGC)

subzTransparent <- copy(subzones_colours_ref)
subzTransparent[,Col := "#FFFFFF00"]
setnames(subzTransparent,c("bgc","Transparent"))

eda <- dbGetQuery(sppDb,"select * from eda")
eda <- as.data.table(eda)
suitcols <- data.table(Suit = c(1,2,3),Col = c("#443e3dFF","#736e6eFF","#a29f9eFF"))#c("#42CF20FF","#ECCD22FF","#EC0E0EFF")
##climatic suitability colours
zonalOpt <- "#3e6837ff"
wetOpt <- data.table(feasible = c(1,2,3), Col = c("#c24f00ff","#cd804bff","#fbbd92ff"))
splitOpt <- "#df00a9ff"
dryOpt <- data.table(feasible = c(1,2,3), Col = c("#000aa3ff","#565edeff","#8b8fdbff"))

pestOps <- dbGetQuery(sppDb,"select distinct pest from forhealth")[,1]
##legends
climaticLeg <- list(
  labels = c("Climatic Optimum","Wet Site Optimum","Dry Site Optimum","Bimodal Feasibility","Off-site Addition","Removed from CFRG"),
  colours = c(zonalOpt, wetOpt$Col[1], dryOpt$Col[1],splitOpt,"#fbff00ff","#8300ffff"),
  title = "Climatic Feasibility"
)

edaLeg <- list(
  labels = edaFreqCols$FeasVal,
  colours = edaFreqCols$Col,
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

instr_climmap <- tagList(
  h3("Climate Summaries - Map Interface"),
  h4("Map Options"),
  p("The map themes BGCs by their mean or variance for the selected climate variable;
    low values are green, high values are red. Hovering over a BGC shows the BGC name 
    and climatic value; clicking on a BGC shows a boxplot of all time periods for that unit."),
  tags$ol(
    tags$li("Select a time period"),
    tags$li("Select a climate variable"),
    tags$li("Select statistic (mean or variance)"),
    tags$li("Select emmision scenario (only applicable to future periods)")
  ),
  p("Note: The button at the top right of the map allows you to select layers to show, and change
    the base layer.")
)

##frequency rules
freq_rules <- data.table(SMR = c(0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 
                                 7L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L),
                         Feasible = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 
                                      2L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L),
                         FreqCode = c(1, 2, 2, 2, 3, 2, 2, 1, 1, 1, 1, 2, 3, 2, 1, 1, 1, 1, 1, 1, 
                                      1, 1, 1, 1))
