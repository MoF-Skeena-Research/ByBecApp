###Kiri Daust
###May 2021

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
source("FeasAppSource.R")
##connect to database
###Read in climate summary data

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
assCols <- data.frame(assessment = c("Fail","Poor","Fair","Good","Excellent","UN"), 
                        Col = c("#E20000","#FF7B00","#FFEC00","#91FB00","#1DB000","#FF00B4"))
fhCols <- data.frame(hazard = c("High","Moderate","Low"), 
                     Col = c("#D80000","#FFEF01","#0CC200"))

#grRamp <- colorRamp(c(edaMaxCol,edaMinCol),alpha = T) ##colour ramp for gray values
grRamp2 <- colorRamp(c("#443e3dFF","#c0c0c0ff"),alpha = T) ##colour ramp for gray values

##initial input table for adding offsite trial
trialInit <- data.table(plotid = character(1),
                  spp = character(1),
                  numplanted = numeric(1),
                  seedlot = character(1),
                  planted = as.Date("2021-01-01"),
                  project_id = character(1),
                  lat = numeric(1),
                  long = numeric(1),
                  assessment = character(1))

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
##max suitability colours
##BGC colours

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

# Define UI for application that draws a histogram
ui <- navbarPage("By BEC Map",theme = "css/bcgov.css",
                 tabPanel("Tree Feasibility",
                          useShinyalert(),
                          useShinyjs(),
                          fluidPage(
                                  column(3,
                                         h3("Tree Range and Feasibility by BGC"),
                                         actionButton("showinstr","Click To Show Instructions"),
                                         br(),
                                         panel(style = "overflow-y:scroll; max-height: 900px; position:relative; align: centre",
                                               pickerInput("sppPick",
                                                           label = "Select a Species",
                                                           choices = sppList,
                                                           selected = "Fd"),                                         
                                               h4("Map Display"),
                                               checkboxInput("updatedfeas","Show Updated Range and Feasibility",value = F, width = "250px"),
                                               awesomeRadio("type",
                                                            label = "Range",
                                                            choices = c("Range","Climatic Suitability"),
                                                            selected =  "Range", inline = T),
                                               h4("Edatopic Feasibility:"),
                                               girafeOutput("edaplot",height = "350px"),
                                              
                                               checkboxGroupInput("showtrees","Show species plots",choices = c("BC","AB","US"),inline = T),
                                               checkboxGroupInput("trials","Show location of offsite trials",c("AMAT","RESULTS"), inline = T),
                                               dateRangeInput("trialStart","Filter offsite trials by planting date:",
                                                              start = minStart, end = maxStart, format = "yyyy", startview = "year")
                                         )

                                  ),
                                  column(9,
                                         useShinyjs(),
                                         leafletjs_feas,
                                         
                                         #tags$style(type = "text/css", "#map {height: calc(100vh - 250) !important;}"),
                                         leafglOutput("map",height = "70vh"),
                                         br(),
                                         h3("Tree Feasibility Ratings for selected BGC:"),
                                         p("Edit the feasibility values here. When you click submit, 
                                            the updated values will be sent to a database. If you are looking
                                           at updated values, they will be shown with a pink background on the table."),
                                         fluidRow(
                                             uiOutput("tableBGC"),
                                             rHandsontableOutput("hot"),
                                             br(),
                                             hidden(actionBttn("submitdat", label = "Submit Changes!")),
                                             hidden(actionBttn("addspp","Add Species")),
                                             h4("Download feasibility data and updates:"),
                                             downloadButton("downloadFeas")
                                         )
                                  )
                              
                              )
                          ),
                 tabPanel("Off-site Trials",
                          column(3,
                                 h2("Offsite Species Trials"),
                                 checkboxGroupInput("trials2","Show location of offsite trials",c("AMAT","RESULTS")),
                                 dateRangeInput("trialStart2","Filter offsite trials by planting date:",
                                                start = minStart, end = maxStart, format = "yyyy", startview = "year"),
                                 br(),
                                 h3("Add Offsite-Trial"),
                                 rHandsontableOutput("addTrial"),
                                 textInput("trialMod","Enter your initials:"),
                                 actionButton("submitTrial","Submit Trial")
                                 ),
                          column(9,
                                 #tags$style(type = "text/css", "#offsiteMap {height: calc(100vh - 250) !important;}"),
                                 leafglOutput("offsiteMap", height = "70vh"),
                                 h3("Trial Info"),
                                 selectInput("trialSelect",
                                             label = "Select a trial, or click on map",
                                             choices = NULL),
                                 h4("Update assessment in table below:"),
                                 rHandsontableOutput("assIn"),
                                 textInput("assessMod",label = "Enter your initials:"),
                                 actionButton("submitAss","Submit Assessment")
                                 )
                          ),
                 tabPanel("Forest Health",
                          column(3,
                                 h2("Hazard Rating by Tree and Pest"),
                                 selectInput("fhSpp",
                                             label = "Select Host Species",
                                             choices = sppList),
                                 selectInput("pestSpp",
                                             label = "Select Pest",
                                             choices = c("DRN","DRL","IDW"),
                                             multiple = F),
                                 h3("Hazard By BGC"),
                                 rHandsontableOutput("fh_hot_long"),
                                 textInput("fhModLong",label = "Enter your initials:"),
                                 actionButton("submitFHLong","Submit Hazard Updates")
                                 ),
                          column(9,
                                 h3("Pest by Host map"),
                                 leafletjs_fh,
                                 #tags$style(type = "text/css", "#fhMap {height: calc(100vh - 250) !important;}"),
                                 leafletOutput("fhMap", height = "70vh"),
                                 br(),
                                 span(textOutput("pestDatLabel", inline = T),style= "font-size:22px"),
                                 rHandsontableOutput("fh_hot"),
                                 textInput("fhMod",label = "Enter your initials:"),
                                 actionButton("submitFH","Submit Hazard Updates")
                                 )
                          ),
                 tabPanel("Find a BGC",
                          column(2,
                                 selectInput("selectBGC","Select BGC", 
                                             choices = c("None",sort(subzones_colours_ref$BGC)), 
                                             multiple = F,selected = "None")
                                 ),
                          column(12,
                                 span(textOutput("selectedBEC", inline = T),style= "font-size:24px"),
                                 leafletOutput("findBGCMap", height = "80vh")
                                 )
                          )
                          
                 )


server <- function(input, output, session) {
    globalFeas <- reactiveValues(dat = "feasible")
    globalLocation <- reactiveValues(loc = c(-124.72,54.56), zoom = 4)
    globalLeg <- reactiveValues(Legend = climaticLeg)
    globalSelBEC <- reactiveVal()
    globalAddTrial <- reactiveValues(data = trialInit)
    
    observeEvent(input$showinstr,{
        shinyalert(title = "Instructions",html = T,text = instr)
    })
    
    testCanAdd <- function(){
        if(input$type == "Range" & is.null(input$edaplot_selected)){
            return(TRUE)
        }
        return(FALSE)
    }
    
    observeEvent({c(input$type,input$map_polygon_click,input$edaplot_selected)},{
        toggle(id = "addspp", condition = testCanAdd())
    })
    
    observe({
        toggle(id = "submitdat", condition = input$type != "Climatic Suitability")
    })
    
    ##this is the column name in the database
    observeEvent(input$updatedfeas,{
        print("Updating feasibility")
        if(input$updatedfeas){
            globalFeas$dat <- "newfeas"
        }else{
            globalFeas$dat <- "feasible"
        }
        
    }, priority = 20)
    
    output$downloadFeas <- downloadHandler(
        filename = "FeasibilityUpdates.csv",
        content = function(file){
            dat <- dbGetQuery(con,"SELECT bgc,ss_nospace,sppsplit,feasible,newfeas,mod FROM feasorig")
            dat <- as.data.table(dat)
            setnames(dat, old = "sppsplit",new = "spp")
            fwrite(dat, file)
        }
    )
    
    ##base BGC map -- done
    output$map <- renderLeaflet({
        leaflet() %>%
            setView(lng = -122.77222, lat = 51.2665, zoom = 6) %>%
            addProviderTiles(leaflet::providers$CartoDB.PositronNoLabels, group = "Positron",
                             options = leaflet::pathOptions(pane = "mapPane")) %>%
            leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "Satellite",
                                      options = leaflet::pathOptions(pane = "mapPane")) %>%
            leaflet::addProviderTiles(leaflet::providers$OpenStreetMap, group = "OpenStreetMap",
                                      options = leaflet::pathOptions(pane = "mapPane")) %>%
            leaflet::addTiles(
                urlTemplate = paste0("https://api.mapbox.com/styles/v1/", mbsty, "/tiles/{z}/{x}/{y}?access_token=", mbtk),
                attribution = '&#169; <a href="https://www.mapbox.com/feedback/">Mapbox</a>',
                group = "Hillshade",
                options = leaflet::pathOptions(pane = "mapPane")) %>%
            addBGCTiles() %>%
            leaflet::addLayersControl(
                baseGroups = c("Positron","Satellite", "OpenStreetMap","Hillshade"),
                overlayGroups = c("BGCs","Feasibility"),
                position = "topright")
    })
    
    ## find a BGC ##
    output$findBGCMap <- renderLeaflet({
        leaflet() %>%
            setView(lng = -122.77222, lat = 51.2665, zoom = 6) %>%
            addProviderTiles(leaflet::providers$CartoDB.PositronNoLabels, group = "Positron",
                             options = leaflet::pathOptions(pane = "mapPane")) %>%
            leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "Satellite",
                                      options = leaflet::pathOptions(pane = "mapPane")) %>%
            leaflet::addProviderTiles(leaflet::providers$OpenStreetMap, group = "OpenStreetMap",
                                      options = leaflet::pathOptions(pane = "mapPane")) %>%
            addSelectBEC() %>%
            leaflet::addLayersControl(
                baseGroups = c("Positron","Satellite", "OpenStreetMap"),
                overlayGroups = c("BEC"),
                position = "topright")
    })
    
    observeEvent(input$selectBGC,{
        session$sendCustomMessage("highlightBEC",input$selectBGC)
    })
    
    observeEvent(input$becselect_click,{
        output$selectedBEC <- renderText({
           c("Selected BGC: ",
            input$becselect_click)
        })
    })
    
    ##offsite trials####
    output$offsiteMap <- renderLeaflet({
        leaflet() %>%
            setView(lng = -122.77222, lat = 51.2665, zoom = 6) %>%
            addProviderTiles(leaflet::providers$CartoDB.PositronNoLabels, group = "Positron",
                             options = leaflet::pathOptions(pane = "mapPane")) %>%
            leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "Satellite",
                                      options = leaflet::pathOptions(pane = "mapPane")) %>%
            leaflet::addProviderTiles(leaflet::providers$OpenStreetMap, group = "OpenStreetMap",
                                      options = leaflet::pathOptions(pane = "mapPane")) %>%
            addBGCTiles() %>%
            leaflet::addLayersControl(
                baseGroups = c("Positron","Satellite", "OpenStreetMap"),
                overlayGroups = c("BGCs"),
                position = "topright")
    })
    
    observeEvent(input$trialSelect,{
        output$assIn <- renderRHandsontable({
            if(input$trialSelect != ""){
                dat <- dbGetQuery(con,paste0("select plotid,spp,numplanted,seedlot,assessment from offsite where plotid = '",
                                             input$trialSelect,"'"))
                dat <- unique(as.data.table(dat))
                rhandsontable(dat) %>%
                    hot_col(col = "assessment",type = "dropdown",source = c("Fail","Poor","Fair","Good","Excellent","UN","Oink"))
            }
        })
    })
    
    observeEvent(input$submitAss,{
        dat <- as.data.table(hot_to_r(input$assIn))
        dat[,mod := input$assessMod]
        dbWriteTable(con, "temp_ass_update", dat, overwrite = T)
        dbExecute(con,"UPDATE offsite
                  SET assessment = temp_ass_update.assessment,
                  mod = temp_ass_update.mod
                  FROM temp_ass_update
                  WHERE offsite.plotid = temp_ass_update.plotid
                  AND offsite.spp = temp_ass_update.spp
                  AND offsite.seedlot = temp_ass_update.seedlot")
        shinyalert("Thank you!","Your updates have been recorded", type = "info", inputId = "assMessage")
    })
    
    observeEvent(input$offsiteMap_glify_click,{
        val <- input$offsiteMap_glify_click$data
        pattern <- "Name:\\s*(.*?)\\s*<br>"
        nme <- regmatches(val,regexec(pattern,val))[[1]][2]
        updateSelectInput(session,"trialSelect",selected = nme)
    })
    
    output$addTrial <- renderRHandsontable({
        rhandsontable(globalAddTrial$data)
    })
    
    observeEvent(input$offsiteMap_click,{
        dat <- as.data.table(hot_to_r(input$addTrial))
        dat$lat <- input$offsiteMap_click$lat
        dat$long <- input$offsiteMap_click$lng
        globalAddTrial$data <- dat
    })
    
    observeEvent(input$submitTrial,{
        dat <- as.data.table(hot_to_r(input$addTrial))
        globalAddTrial$data <- trialInit
        dat$mod <- input$trialMod
        dat <- st_as_sf(dat, coords = c("long","lat"), crs = 4326)
        st_write(dat, con, "offsite", append = TRUE)
        shinyalert("Thank you!","Your trial has been recorded", type = "info", inputId = "trialMessage")
    })
    
    observeEvent({c(input$trials2,
                    input$trialStart2)},{
                        if(!is.null(input$trials2)){
                            dat2 <- st_read(con,query = paste0("select project_id, plotid, spp, seedlot,assessment, geometry from offsite where project_id in ('",paste(input$trials2,collapse = "','"),
                                                               "') and planted > '", input$trialStart2[1],"' and planted < '",input$trialStart2[2],"'"))
                            if(nrow(dat2) == 0){
                                dat2 <- NULL
                            }else{
                                dat2$label <- paste0("Name: ",dat2$plotid,"<br>Seedlot: ",dat2$seedlot)
                                updateSelectInput(session,"trialSelect",choices = unique(dat2$plotid))
                                dat2 <- dat2[,c("project_id","label","assessment")]
                                colnames(dat2)[1] <- "region"
                                dat2 <- merge(dat2, assCols, by = "assessment")
                                dat2$Col <- as.character(dat2$Col)
                                leafletProxy("offsiteMap") %>%
                                    addGlPoints(data = dat2,layerId = "tree_trial",popup = ~ label,
                                                fillColor = dat2$Col,fragmentShaderSource = "point")
                            }
                        }else{
                            leafletProxy("offsiteMap") %>%
                                removeGlPoints("tree_trial")
                        }
                    })
    ###end offsite tab##
    
    ### start for health tab ###
    observeEvent(input$fhSpp,{
        treeSpp <- substr(input$fhSpp,1,2)
        dat <- dbGetQuery(con,paste0("select distinct pest,pest_name from forhealth where treecode like '",treeSpp,"'"))
        dat$pest_name <- paste0(dat$pest," - ", dat$pest_name)
        if(nrow(dat) == 0){
            updateSelectInput(session, "pestSpp", choices = "")
        }else{
            temp <- dat$pest
            names(temp) <- dat$pest_name
            updateSelectInput(session, "pestSpp", choices = temp)
        }
    })
    
    output$fhMap <- renderLeaflet({
        leaflet() %>%
            setView(lng = -122.77222, lat = 51.2665, zoom = 6) %>%
            addProviderTiles(leaflet::providers$CartoDB.PositronNoLabels, group = "Positron",
                             options = leaflet::pathOptions(pane = "mapPane")) %>%
            leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "Satellite",
                                      options = leaflet::pathOptions(pane = "mapPane")) %>%
            leaflet::addProviderTiles(leaflet::providers$OpenStreetMap, group = "OpenStreetMap",
                                      options = leaflet::pathOptions(pane = "mapPane")) %>%
            addBGCTiles() %>%
            leaflet::addLayersControl(
                baseGroups = c("Positron","Satellite", "OpenStreetMap"),
                overlayGroups = c("BGCs"),
                position = "topright") %>%
            addLegend(position = "bottomright",
                       labels = c("Unspecified","Low","Moderate","High","Outside Range"),
                       colors = c("#443e3d","#0CC200","#FFEF01","#D80000","#840090"),
                       title = "Hazard",
                       layerId = "bec_fh")
    })
    
    prepDatFH <- reactive({
        QRY <- paste0("select bgc,ss_nospace,sppsplit,spp,feasible from feasorig where spp = '",
                      substr(input$fhSpp,1,2),"' and feasible in (1,2,3,4,5)")
        d1 <- dbGetQuery(con, QRY)
        if(nrow(d1) == 0){
            shinyalert(title = "Oops!",text = "There are no data for that species",
                       type = "error",showConfirmButton = T)
            QRY <- paste0("select bgc,ss_nospace,sppsplit,spp,feasible from feasorig where spp = 'Sx'")
            d1 <- dbGetQuery(con, QRY)
        }
        feas <- as.data.table(d1)
        feasMax <- feas[,.(SuitMax = min(feasible)), by = .(bgc,spp)]
        feasMax[,Col := "#443e3dFF"]
        feasMax[,Lab := bgc]
        feasMax[,.(bgc,Col,Lab)]
    })
    
    observeEvent(input$pestSpp,{
        dat <- dbGetQuery(con,paste0("select bgc,hazard_update from forhealth where treecode like '",
                                     substr(input$fhSpp,1,2),"' and pest = '",input$pestSpp,
                                     "' and hazard_update <> 'UN'"))
        dat <- as.data.table(dat)
        #browser()
        setnames(dat, old = "hazard_update", new = "hazard")
        dat[fhCols,fhcol := i.Col, on = "hazard"]
        rangeDat <- prepDatFH()
        dat[rangeDat, InRange := i.Col, on = "bgc"]
        dat[is.na(InRange), fhcol := "#840090"]
        session$sendCustomMessage("colourPest",dat[,.(bgc,fhcol)])
    })
    
    observeEvent({c(
        input$fhSpp)
    },{
        dat <- prepDatFH()
        if(nrow(dat) == 0){
            dat <- NULL
        }
        print("Rendering map")
        dat <- dat[subzTransparent, on = "bgc"]
        dat[is.na(Col),Col := Transparent]
        dat[is.na(Lab),Lab := bgc]
        if(!is.null(dat)){
            leafletProxy("fhMap") %>%
                invokeMethod(data = dat, method = "addFHTiles", dat$bgc, dat$Col,dat$Lab)
        }
        
    },priority = 10)
    
    observeEvent(input$fh_click,{
        output$pestDatLabel <- renderText({
            paste0("Pest Table for ",input$fh_click)
        })
    })
    
    observeEvent(input$fh_click,{
        if(!is.null(input$fh_click)){
                dat1 <- dbGetQuery(con,paste0("select treecode, pest, pest_name, bgc, hazard, hazard_update 
                                          from forhealth where bgc = '",input$fh_click,"'"))
                dat <- as.data.table(dat1)
                if(nrow(dat) < 1){
                    dat <- data.table()
                    col_num <- NULL
                    row_num <- NULL
                }else{
                    dat <- dcast(dat, treecode ~ pest, value.var = "hazard_update", 
                                 fun.aggregate = function(x) x[1])
                    col_num <- which(colnames(dat) == input$pestSpp) - 1
                    row_num <- which(dat$treecode == substr(input$fhSpp,1,2)) - 1
                }
                output$fh_hot <- renderRHandsontable({
                    rhandsontable(dat,col_highlight = col_num, 
                                  row_highlight = row_num) %>%
                        hot_cols(type = "autocomplete",source = c("Low","Moderate","High","UN"),
                                 renderer = 
                                     "function(instance, td, row, col, prop, value, cellProperties) {
                                          Handsontable.renderers.TextRenderer.apply(this, arguments);
                                          
                                          if(instance.params) {
                                            hcols = instance.params.col_highlight
                                            hcols = hcols instanceof Array ? hcols : [hcols]
                                            hrows = instance.params.row_highlight
                                            hrows = hrows instanceof Array ? hrows : [hrows]
                                          }
                                        if (instance.params && (col === hcols[0])) {
                                          td.style.background = 'pink';
                                        }
                                        if (instance.params && (row === hrows[0])) {
                                          td.style.background = 'lightgreen';
                                        }
                                        if (instance.params && (col === hcols[0] && row === hrows[0])) {
                                          td.style.background = 'yellow';
                                        }
                                      }"
                                     )
                })
            }
    })
    
    observeEvent({c(input$pestSpp,
                    input$fhSpp)},{
        dat <- dbGetQuery(con,paste0("select distinct bgc, hazard, hazard_update from forhealth where treecode = '",
                          substr(input$fhSpp,1,2),"' and pest = '",input$pestSpp,"'"))
        if(nrow(dat) > 0){
            output$fh_hot_long <- renderRHandsontable({
                rhandsontable(dat) %>%
                    hot_col("hazard_update",type = "autocomplete",source = c("Low","Moderate","High","UN"),strict = T)
            })
        }
    
    })

    observeEvent(input$submitFH,{
        dat <- as.data.table(hot_to_r(input$fh_hot))
        dat <- melt(dat,id.vars = "treecode",variable.name = "pest",value.name = "hazard_update")
        dat[,mod := input$fhMod]
        dat[,bgc := input$fh_click]
        if(nrow(dat) > 0){
            dbWriteTable(con, "temp_fh", dat, overwrite = T,row.names = F)
            dbExecute(con,"UPDATE forhealth
                  SET hazard_update = temp_fh.hazard_update,
                  mod = temp_fh.mod
                  FROM temp_fh
                  WHERE forhealth.treecode = temp_fh.treecode
                  AND forhealth.pest = temp_fh.pest
                  AND forhealth.bgc = temp_fh.bgc")
        }
        shinyalert("Thank you!","Your updates have been recorded", type = "info", inputId = "fhMessage")
    })
    
    observeEvent(input$submitFHLong,{
        dat <- as.data.table(hot_to_r(input$fh_hot_long))
        dat[,`:=`(mod = input$fhModLong,treecode = substr(input$fhSpp,1,2),
                  pest = input$pestSpp)]
        if(nrow(dat) > 0){
            dbWriteTable(con, "temp_fh", dat, overwrite = T,row.names = F)
            dbExecute(con,"UPDATE forhealth
                  SET hazard_update = temp_fh.hazard_update,
                  mod = temp_fh.mod
                  FROM temp_fh
                  WHERE forhealth.treecode = temp_fh.treecode
                  AND forhealth.pest = temp_fh.pest
                  AND forhealth.bgc = temp_fh.bgc")
        }
        shinyalert("Thank you!","Your updates have been recorded", type = "info", inputId = "fhMessage")
    })
    
    
    ##end for health tab###
    
    observeEvent({c(input$showtrees,
                    input$sppPick,
                    input$trials,
                    input$trialStart)},{
                        sppName <- substr(input$sppPick,1,2)
                        if(!is.null(input$showtrees)){
                            QRY <- paste0("select spp,plotnum,geometry from plotdata where spp = '",sppName,"' and region in ('",
                                          paste(input$showtrees,collapse = "','"),"')")
                            dat <- st_read(con,query = QRY)
                            dat <- dat["plotnum"]
                            colnames(dat)[1] <- "label"
                            leafletProxy("map") %>%
                                addGlPoints(data = dat,layerId = "tree_plot",popup = ~ label,
                                            fillColor = "#2DB000",fragmentShaderSource = "point")
                        }else{
                            leafletProxy("map") %>%
                                removeGlPoints("tree_plot")
                        }
                        if(!is.null(input$trials)){
                            dat2 <- st_read(con,query = paste0("select project_id, plotid, spp, seedlot,assessment, geometry from offsite where spp like '",
                                                       sppName,"%' and project_id in ('",paste(input$trials,collapse = "','"),
                                                       "') and planted > '", input$trialStart[1],"' and planted < '",input$trialStart[2],"'"))
                            if(nrow(dat2) == 0){
                                dat2 <- NULL
                            }else{
                                dat2$label <- paste0("Name: ",dat2$plotid,"<br>Seedlot: ",dat2$seedlot)
                                updateSelectInput(session,"trialSelect",choices = unique(dat2$plotid))
                                dat2 <- dat2[,c("project_id","label","assessment")]
                                colnames(dat2)[1] <- "region"
                                dat2 <- merge(dat2, assCols, by = "assessment")
                                dat2$Col <- as.character(dat2$Col)
                                leafletProxy("map") %>%
                                    addGlPoints(data = dat2,layerId = "tree_trial",popup = ~ label,
                                                fillColor = dat2$Col,fragmentShaderSource = "square")
                            }
                        }else{
                            leafletProxy("map") %>%
                                removeGlPoints("tree_trial")
                        }
    })
    
    ##Prepare BGC colour table for non-edatopic
    prepDatSimple <- reactive({
        QRY <- paste0("select bgc,ss_nospace,sppsplit,spp,",globalFeas$dat,
                      " from feasorig where spp = '",substr(input$sppPick,1,2),
                      "' and ",globalFeas$dat," in (1,2,3,4,5)")
        d1 <- tryCatch({
            dbGetQuery(con, QRY)
        },
        error = function(e){
            invisible(lapply(dbListConnections(PostgreSQL()), dbDisconnect))
            con <<- dbConnect(drv, user = "postgres", password = "Kiriliny41", host = "68.183.199.104", 
                              port = 5432, dbname = "spp_feas")
            dat <- dbGetQuery(con, QRY)
            return(dat)
        })
        if(nrow(d1) == 0){
            shinyalert(title = "Oops!",text = "There are no data for that species",
                       type = "error",showConfirmButton = T)
            QRY <- paste0("select bgc,ss_nospace,sppsplit,spp,",globalFeas$dat,
                          " from feasorig where spp = 'Sx' and ",globalFeas$dat," in (1,2,3,4,5)")
            d1 <- dbGetQuery(con, QRY)
        }
        feas <- as.data.table(d1)
        setnames(feas, old = globalFeas$dat, new = "feasible")
        feasMax <- feas[,.(SuitMax = min(feasible)), by = .(bgc,sppsplit)]
        if(input$type == "Range"){
            if(length(unique(feasMax$sppsplit)) > 1){
                feasMax[,SppNum := as.numeric(as.factor(sppsplit))]
                tempCol <- grRamp2(rescale(feasMax$SppNum,to = c(0,0.6)))
                feasMax[,Col := rgb(tempCol[,1],tempCol[,2],tempCol[,3],tempCol[,4], maxColorValue = 255)]
                temp <- unique(feasMax[,.(sppsplit,Col)])
                
                PALeg <- list(
                    labels = c(temp$sppsplit,"Added","Removed"),
                    colours = c(temp$Col,"#fbff00ff","#8300ffff"),
                    title = "Presence/Absence"
                )
                globalLeg$Legend <- PALeg
                
                feasMax[SuitMax == 4,Col := "#fbff00ff"]
                feasMax[SuitMax == 5,Col := "#8300ffff"]
            }else{
                feasMax[,Col := "#443e3dFF"]
                feasMax[SuitMax == 4,Col := "#fbff00ff"]
                feasMax[SuitMax == 5,Col := "#8300ffff"]
                PALeg <- list(
                    labels = c(input$sppPick,"Added","Removed"),
                    colours = c("#443e3dFF","#fbff00ff","#8300ffff"),
                    title = "Presence/Absence"
                )
                globalLeg$Legend <- PALeg
            }
        }else if(input$type == "Max Suit"){
            feasMax[suitcols, Col := i.Col, on = c(SuitMax = "Suit")]
            #globalLeg$Legend <- maxSuitLeg
        }else{
            feasMax <- prepClimSuit()
            globalLeg$Legend <- climaticLeg
        }
        feasMax[,Lab := bgc]
        feasMax[,.(bgc,Col,Lab)]
        
    })
    
    ##Prepare BGC colours for edatopic option
    prepEdaDat <- reactive({
        QRY <- paste0("select bgc,ss_nospace,sppsplit,spp,",globalFeas$dat,
                      " from feasorig where spp = '",substr(input$sppPick,1,2),
                      "' and ",globalFeas$dat," in (1,2,3,4)")
        feas <- as.data.table(dbGetQuery(con, QRY))
        setnames(feas, old = globalFeas$dat, new = "feasible")        
        globalLeg$Legend <- edaLeg
        id <- as.numeric(input$edaplot_selected)
        idSub <- idDat[ID == id,.(ID,edatopic)]
        edaSub <- eda[idSub, on = "edatopic"]
        feasSub <- feas[ss_nospace %chin% edaSub$ss_nospace,]
        feasSub[,Lab := paste0(ss_nospace,": ", feasible)]
        feasSum <- feasSub[,.(FeasVal = mean(feasible), Lab = paste(Lab, collapse = "<br>")), by = bgc]
        #tempCol <- grRamp(rescale(feasSum$FeasVal,to = c(0,1)))
        feasSum[,Col := colour_values(rescale(feasSum$FeasVal,to = c(0,1)))]
        feasSum[,.(bgc,Col,Lab)]
    })
    
    ###calculate climatic suitability colours
    prepClimSuit <- reactive({
        QRY <- paste0("select bgc,ss_nospace,sppsplit,spp,",globalFeas$dat,
                      " from feasorig where spp = '",substr(input$sppPick,1,2),
                      "' and ",globalFeas$dat," in (1,2,3,4,5)")
        feas <- as.data.table(dbGetQuery(con, QRY))
        setnames(feas, old = globalFeas$dat, new = "feasible")
        tempFeas <- feas[feasible %in% c(1,2,3),]
        minDist <- tempFeas[,.SD[feasible == min(feasible, na.rm = T)],by = .(bgc)]
        abUnits <- minDist[grep("[[:alpha:]] */[[:alpha:]]+$",ss_nospace),]
        noAb <- minDist[!grepl("[[:alpha:]] */[[:alpha:]]+$",ss_nospace),]
        abUnits <- eda[abUnits, on = "ss_nospace"] ##merge
        abUnits <- abUnits[,.(Temp = if(any(grepl("C4",edatopic))) paste0(ss_nospace,"_01") else ss_nospace, feasible = feasible[1]),
                           by = .(bgc,ss_nospace,sppsplit,spp)]
        abUnits[,ss_nospace := NULL]
        setnames(abUnits,old = "Temp",new = "ss_nospace")
        minDist <- rbind(noAb,abUnits)
        minDist[,ID := if(any(grepl("01", ss_nospace)) & feasible[1] == 1) T else F, by = .(bgc)]
        green <- minDist[(ID),]
        green <- green[,.(Col = zonalOpt), by = .(bgc)]
        
        minDist <- minDist[ID == F,]
        minDist[,ID := if(any(grepl("01", ss_nospace))) T else F, by = .(bgc)]
        blue <- minDist[(ID),]
        blue <- blue[,.(feasible = min(feasible)), by = .(bgc)]
        
        minDist <- minDist[ID == F,]
        minEda <- eda[minDist, on = "ss_nospace"]
        minEda <- minEda[,.(AvgEda = mean(smr)), by = .(bgc,ss_nospace,feasible)]
        minEda <- minEda[,.(Col = fifelse(all(AvgEda >= 3.5),"WET",
                                          fifelse(all(AvgEda < 3.5), "DRY", splitOpt)), feasible = min(feasible)), by = .(bgc)]
        temp <- minEda[Col == "DRY",]
        temp[,Col := NULL]
        blue <- rbind(blue,temp)
        red <- minEda[Col == "WET",]
        minEda <- minEda[!Col %in% c("WET","DRY"),.(bgc,Col)]
        blue[dryOpt, Col := i.Col, on = "feasible"]
        red[wetOpt, Col := i.Col, on = "feasible"]
        blue[,feasible := NULL]
        red[,feasible := NULL]
        climSuit <- rbind(green,blue,red,minEda)
        climSuit <- climSuit[!is.na(bgc),]
        
        tf2 <- feas[feasible %in% c(4,5),.(SuitMax = min(feasible)), by = .(bgc)]
        if(nrow(tf2) > 0){
            tf2[SuitMax == 4,Col := "#fbff00ff"]
            tf2[SuitMax == 5,Col := "#8300ffff"]
            tf2 <- tf2[,.(bgc,Col)]
            climSuit <- rbind(climSuit, tf2)
        }
        return(climSuit)
    })

    observeEvent({c(
        input$sppPick,
        input$type,
        input$edaplot_selected,
        input$updatedfeas)
    },{
    if(is.null(input$edaplot_selected)){
        dat <- prepDatSimple()
    }else{
        dat <- prepEdaDat()
    }
    if(nrow(dat) == 0){
        dat <- NULL
    }
    print("Rendering map")
    dat <- dat[subzTransparent, on = "bgc"]
    dat[is.na(Col),Col := Transparent]
    dat[is.na(Lab),Lab := bgc]
    if(!is.null(dat)){
        leafletProxy("map") %>%
            invokeMethod(data = dat, method = "addGridTiles", dat$bgc, dat$Col,dat$Lab) %>%
            addLegend(position = "bottomright",
                      labels = globalLeg$Legend$labels,
                      colors = globalLeg$Legend$colours,
                      title = globalLeg$Legend$title,
                      layerId = "bec_feas")
    }else{
        leafletProxy("map") %>%
            removeShape(layer_id = "BECMap")
    }

}, priority = 15)

    output$tableBGC <- renderUI({
        unit <- globalSelBEC()
        if(!is.null(unit)){
            tagList(
                h3(paste0("Feasibility for ",unit)),
                br()
            )
        }
    })
    
    ##prepare suitability table when polygon clicked
    prepTable <- reactive({
        unit <- globalSelBEC()
        print(unit)
        idx_row <- NULL
        idx_col <- NULL
        QRY <- paste0("select bgc,ss_nospace,sppsplit,spp,",globalFeas$dat,
                      " from feasorig where bgc = '",unit,"' and ",globalFeas$dat," in (1,2,3,4)")
        feas <- as.data.table(dbGetQuery(con, QRY))
        if(nrow(feas) == 0){
            shinyalert("Oopsies!","There are no species in that subzone :(")
            return(list(dat = feas, rIdx = NULL, cIdx = NULL, sppCol = NULL))
        }
        setnames(feas, old = globalFeas$dat, new = "feasible")   
        if(is.null(input$edaplot_selected)){
            if(input$type == "Climatic Suitability"){
                tempFeas <- feas[feasible %in% c(1,2,3),]
                tempEda <- eda[tempFeas, on = "ss_nospace"]
                tempEda <- tempEda[!is.na(smr),]
                tempEda <- tempEda[,.(AvgSMR = mean(smr)), by = .(ss_nospace,feasible,sppsplit)]
                tempEda[,SSType := fifelse(grepl("01",ss_nospace),"Zonal",fifelse(AvgSMR <= 3.5,"Dry",
                                                                                  fifelse(AvgSMR > 4.1,"Wet","??")))]
                tabOut <- dcast(tempEda, SSType ~ sppsplit, value.var = "feasible", fun.aggregate = min)
                tabOut[tabOut == 0] <- NA
            }else{
                feasSub <- feas[sppsplit != "X",]
                tabOut <- data.table::dcast(feasSub, ss_nospace ~ sppsplit,fun.aggregate = mean, value.var = "feasible")
                tabOut[,lapply(.SD,as.integer),.SDcols = -"ss_nospace"]
            }
        }else{
            id <- as.numeric(input$edaplot_selected)
            idSub <- idDat[ID == id,.(ID,edatopic)]
            edaSub <- eda[idSub, on = "edatopic"]
            edaSub <- edaSub[bgc == unit,]
            dat <- feas[ss_nospace %in% edaSub$ss_nospace & feasible %in% c(1,2,3,4),]
            tabOut <- data.table::dcast(dat, ss_nospace ~ sppsplit, value.var = "feasible", fun.aggregate = mean)
            tabOut[,lapply(.SD,as.integer),.SDcols = -"ss_nospace"]
            if(input$updatedfeas){
                QRY <- paste0("select ss_nospace,sppsplit,feasible from feasorig where bgc = '",
                              unit,"' and feasible in (1,2,3,4)")
                feasOrig <- as.data.table(dbGetQuery(con, QRY))
                dat2 <- feasOrig[ss_nospace %in% edaSub$ss_nospace,]
                setnames(dat2, old = "feasible", new = "FeasOld")
                comp <- merge(dat,dat2,on = c("ss_nospace","sppsplit"),all = T)
                comp[,Same := (feasible == FeasOld) & !is.na(feasible) & !is.na(FeasOld)]
                tabOrig <- data.table::dcast(comp, ss_nospace ~ sppsplit, value.var = "Same",fun.aggregate = function(x){x[1]})
                idx <- which(tabOrig == F, arr.ind = T)
                idx_row <- unname(idx[,1] - 1)
                idx_col <- unname(idx[,2] - 1)
            }
            
        }
        spp <- colnames(tabOut)
        spp[spp %in% c("Se","Sw","Sxw")] <- "Sx"
        spp[spp %in% c("Sxl","Sxs","Ss")] <- "Ss"        
        spp <- substr(spp, 1,2)
        sppCurr <- substr(input$sppPick,1,2)
        if(sppCurr %in% spp){
            sppIdx <- which(spp == sppCurr) - 1
        }else{
            sppIdx <- NULL
        }
        list(dat = tabOut, rIdx = idx_row, cIdx = idx_col, sppCol = sppIdx)
    })
    
    observeEvent(input$bgc_click,{
        #updateSelectInput(session, "selectBGC",selected = "None")
        globalSelBEC(input$bgc_click)
    })
    
    ##render suitability table, colour updated cells
    observeEvent({c(input$bgc_click, 
                    input$edaplot_selected,
                    input$sppPick,
                    input$updatedfeas,
                    input$selectBGC)},{
        if(!is.null(globalSelBEC())){
            output$hot <- renderRHandsontable({
                temp <- prepTable()
                dat <- temp$dat
                #browser()
                rhandsontable(data = dat,col_highlight = temp$cIdx,
                              row_highlight = temp$rIdx, spp_highlight = temp$sppCol) %>%
                    hot_cols(format = "0", renderer = "
                function(instance, td, row, col, prop, value, cellProperties) {
                Handsontable.renderers.NumericRenderer.apply(this, arguments);
                if (instance.params) {
                    hcols = instance.params.col_highlight
                    hcols = hcols instanceof Array ? hcols : [hcols]
                    hrows = instance.params.row_highlight
                    hrows = hrows instanceof Array ? hrows : [hrows]
                    hspp = instance.params.spp_highlight
                    hspp = hspp instanceof Array ? hspp : [hspp]
                }
                
                var i;
                for(i = 0; i < 100; i++){
                    if (instance.params && (col === hcols[i] && row === hrows[i])) {
                      td.style.background = 'yellow';
                    }
                    if(instance.params && col === hspp[i]){
                        td.style.background = 'lightgreen';
                    }
                }
                    
            }
                             ")
            })
        }
    })
    
    ##ask for initials and call sendToDb
    observeEvent(input$submitdat,{
        shinyalert("Enter your initials:", type = "input", inputId = "initials", callbackR = sendToDb)
    })
        
    ##compile and send updates to database
    sendToDb <- function(nme){
        dat <- as.data.table(hot_to_r(input$hot))
        dat <- melt(dat, id.vars = "ss_nospace", value.name = "newfeas", variable.name = "sppsplit")
        dat[,mod := nme]
        dbWriteTable(con, "temp_update", dat, overwrite = T)
        dbExecute(con,"UPDATE feasorig 
                  SET newfeas = temp_update.newfeas,
                  mod = temp_update.mod
                  FROM temp_update
                  WHERE feasorig.ss_nospace = temp_update.ss_nospace
                  AND feasorig.sppsplit = temp_update.sppsplit")
        dbExecute(con,"UPDATE feasorig
                  SET newfeas = 5
                  WHERE newfeas IS NULL
                  AND feasible IS NOT NULL")
        shinyalert("Thank you!","Your updates have been recorded", type = "info", inputId = "dbmessage")

    }
    
    output$hot_add <- renderRHandsontable({
        if(!is.null(globalSelBEC())){
            unit <- globalSelBEC()
            edaSub <- unique(eda[bgc == unit,.(bgc,ss_nospace)])
            temp <- data.table(ss_nospace = edaSub$ss_nospace, newfeas = NA_integer_)
            rhandsontable(data = temp)
        }
    })
    
    observeEvent(input$addspp,{
        shinyalert(html = T,
                   text = tagList(
                       h4("Select a species, add feasibility, then click submit"),
                       pickerInput("sppPickAdd",
                                   label = "",
                                   choices = allSppNames,
                                   selected = "Ba"), 
                       fluidRow(column(6,rHandsontableOutput("hot_add")),
                                column(6,textInput("addsppMod",label = "Enter your initials:"))),
                       
                   ),
                   callbackR = addSppToDb,
                   showCancelButton = T,
                   showConfirmButton = T)
    })
    
    addSppToDb <- function(x){
        if(x){
            dat <- hot_to_r(input$hot_add)
            dat <- as.data.table(dat)
            dat2 <- data.table(bgc = gsub("/[[:digit:]]*","", dat$ss_nospace),
                               ss_nospace = dat$ss_nospace,
                               sppsplit = input$sppPickAdd,
                               feasible = NA,spp = substr(input$sppPickAdd,1,2), newfeas = dat$newfeas, mod = input$addsppMod)
            
            dat2 <- dat2[!is.na(newfeas),]
            dbWriteTable(con, name = "feasorig", value = dat2, append = T,row.names = F)
            shinyalert("Thank you!","Your updates have been recorded", type = "info", inputId = "dbmessage")
        }
    }

    ##render interactive edatopic grid
    output$edaplot <- renderGirafe({
        gg <- ggplot()+
            geom_blank()+
            scale_y_discrete(limits = c("7","6","5","4","3","2","1","0"))+
            scale_x_discrete(limits = c("A","B","C","D","E"))+
            geom_rect_interactive(data = rects, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, 
                                                    data_id = ids), 
                                  fill = "grey", col = "purple")+
            geom_hline(aes(yintercept = grd1y), linetype = "dashed")+
            geom_vline(aes(xintercept = grd1x), linetype = "dashed")+
            theme_bw(base_size = 16)+
            theme(panel.grid.major = element_blank())+
            labs(x = "SNR", y = "SMR")+
            coord_fixed()
        
        girafe(ggobj = gg,
               options = list(opts_selection(type = "single")),
               width_svg = 4, height_svg = 7)
    })
    
    
    ##table caption
    output$tableInfo <- renderText({
        if(is.null(input$edaplot_selected)){
            if(input$type == "Climatic Suitability"){
                "Zonal feasibility and maximum feasibility in wetter and drier sites"
            }else{
                "Maximum feasibility by subzone"
            }
        }else{
            "Feasibility for each site series overlapping selected edatopic area"
        }
    })
    
    onStop(function() {
        dbDisconnect(conn = con)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
