###Kiri Daust
###June 2021
source("AppSetup.R") ##load a prepare data
source("FeasAppSource.R") ##javascript functions
# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("lumen"),
                fluidRow(style = "background-color: #003366;",
                    column(6,img(src = "images/gov3_bc_logo.png",align = "left")),
                         column(6,h1("By BEC Map",style = "color: white;"))),
                tabsetPanel(
                    tabPanel("Tree Feasibility",
                             useShinyalert(),
                             useShinyjs(),
                             fluidPage(
                                 column(3,
                                        h3("Tree Range and Feasibility by BGC"),
                                        br(),
                                        panel(style = "overflow-y:scroll; max-height: 900px; position:relative; align: centre",
                                              pickerInput("sppPick",
                                                          label = "Select a Species",
                                                          choices = c("None",sppList),
                                                          selected = "Cw - western redcedar"),                                         
                                              h4("Map Display"),
                                              checkboxInput("updatedfeas","Show Updated Range and Feasibility",value = F, width = "250px"),
                                              checkboxInput("showFreq","Show Frequency",value = T),
                                              h4("Edatopic Feasibility:"),
                                              girafeOutput("edaplot",height = "350px"),
                                              
                                              checkboxGroupInput("showtrees","Show species plots",choices = c("BC","AB","US"),inline = T),
                                              checkboxGroupInput("trials","Show location of offsite trials",c("AMAT","RESULTS"), inline = T),
                                              sliderInput("trialStart","Filter offsite trials by planting date:",
                                                          min = minStart, max = maxStart, value = c(minStart,maxStart))
                                        )
                                        
                                 ),
                                 column(9,
                                        useShinyjs(),
                                        leafletjs_feas,
                                        
                                        #tags$style(type = "text/css", "#map {height: calc(100vh - 250) !important;}"),
                                        actionButton("showinstr","Click To Show Instructions"),
                                        br(),
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
                                    checkboxGroupInput("trials2","Show location of offsite trials",offsiteProj),
                                    h3("Filters"),
                                    pickerInput("sppPick2",
                                                label = "Select a Species",
                                                choices = c("All",sppList),
                                                selected = "All"),
                                    checkboxInput("multiSppTrial","Only show multi-species trials"),
                                    sliderInput("trialStart2","Filter offsite trials by planting date:",
                                                min = minStart, max = maxStart, value = c(minStart,maxStart)),
                                    br(),
                                    h3("Add Offsite-Trial"),
                                    splitLayout(
                                        selectInput("addTr_proj",label = "Choose Project Name",choices = offsiteProj,multiple = F),
                                        textInput("addTr_id",label = "Enter trial id"),
                                        dateInput("addTr_planted",label = "Enter date planted")
                                    ),
                                    h4("Enter location or click on map:"),
                                    splitLayout(
                                        textInput("addTr_lat","Latitude"),
                                        textInput("addTr_long","Longitude")
                                    ),
                                    rHandsontableOutput("addTrial"),
                                    textInput("trialMod","Enter your initials:"),
                                    actionButton("submitTrial","Submit Trial")
                             ),
                             column(9,
                                    leafglOutput("offsiteMap", height = "70vh"),
                                    h3("Trial Info"),
                                    selectInput("trialSelect",
                                                label = "Select a trial, or click on map",
                                                choices = NULL),
                                    h4("Update assessment in table below:"),
                                    rHandsontableOutput("assIn"),
                                    textInput("assessMod",label = "Enter your initials:"),
                                    actionButton("submitAss","Submit Assessment"),
                                    br(),
                                    h4("Download pest data and updates:"),
                                    downloadButton("downloadPest")
                             )
                    ),
                    tabPanel("Forest Health",
                             column(3,
                                    h2("Hazard Rating by Tree and Pest"),
                                    selectInput("fhSpp",
                                                label = "Select Host Species",
                                                choices = c("None",sppList)),
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
                                    p("Note: to add a new pest, right-click on the table, select 'add row', and enter 
                                   pest name, code, and hazard ratings."),
                                    rHandsontableOutput("fh_hot"),
                                    textInput("fhMod",label = "Enter your initials:"),
                                    actionButton("submitFH","Submit Hazard Updates")
                             )
                    ),
                    tabPanel("Find a BGC",
                             fluidRow(
                                 column(2,
                                        selectInput("selectBGC","Select Zone", 
                                                    choices = c("(N)",zones), 
                                                    multiple = F,selected = "(N)"),
                                        pickerInput("selectSubzone","Select subzone(s)",
                                                    choices = "",multiple = T, options =  list(
                                                        `actions-box` = TRUE,
                                                        size = 10)
                                        ),
                                        span(textOutput("selectedBEC", inline = T),style= "font-size:24px")
                                 ),
                                 column(10,
                                        leafletOutput("findBGCMap", height = "80vh")
                                 )
                             )
                             
                    ),
                    tabPanel("About",
                             panel(style = "overflow-y:scroll; max-height: 900px; position:relative; align: centre",
                                   h1("About the By BGC Map Apps"),
                                   p("This site houses a collection of apps all centered around 
                                     using the BEC system. Probably Will wants to put something here. BEC
                                     is good, BEC is great, BEC might even be god. Should we explain the 
                                     apps here or have instructions on their respective pages?", style = "font-size:18px"),
                                   hr(),
                                   h3("Authors"),
                                   p("Site Author: Kiri Daust"),
                                   p("Content Author: Will MacKenzie"),
                                   p("Please submit issues or PRs to our", a("Github repo",href = "https://github.com/FLNRO-Smithers-Research/ByBecApp"))
                                   )
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
        if(is.null(input$edaplot_selected)){
            return(TRUE)
        }
        return(FALSE)
    }
    
    observeEvent({c(input$map_polygon_click,input$edaplot_selected)},{
        toggle(id = "addspp", condition = testCanAdd())
    })
    
    observe({
        toggle(id = "submitdat", condition = input$sppPick != "None")
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
    
    output$downloadPest <- downloadHandler(
        filename = "PestUpdates.csv",
        content = function(file){
            dat <- dbGetQuery(con,"SELECT * FROM forhealth")
            dat <- as.data.table(dat)
            fwrite(dat, file)
        }
    )
    
    ##base BGC map -- done
    output$map <- renderLeaflet({
        leaflet() %>%
            setView(lng = -122.77222, lat = 51.2665, zoom = 6) %>%
            leaflet::addTiles(
                urlTemplate = paste0("https://api.mapbox.com/styles/v1/", mbsty, "/tiles/{z}/{x}/{y}?access_token=", mbtk),
                attribution = '&#169; <a href="https://www.mapbox.com/feedback/">Mapbox</a>',
                group = "Hillshade",
                options = leaflet::pathOptions(pane = "mapPane")) %>%
            addProviderTiles(leaflet::providers$CartoDB.PositronNoLabels, group = "Positron",
                             options = leaflet::pathOptions(pane = "mapPane")) %>%
            leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "Satellite",
                                      options = leaflet::pathOptions(pane = "mapPane")) %>%
            leaflet::addProviderTiles(leaflet::providers$OpenStreetMap, group = "OpenStreetMap",
                                      options = leaflet::pathOptions(pane = "mapPane")) %>%
            leaflet::addTiles(
                urlTemplate = paste0("https://api.mapbox.com/styles/v1/", mblbsty, "/tiles/{z}/{x}/{y}?access_token=", mbtk),
                attribution = '&#169; <a href="https://www.mapbox.com/feedback/">Mapbox</a>',
                group = "Cities",
                options = leaflet::pathOptions(pane = "overlayPane")) %>%
            addBGCTiles() %>%
            leaflet::addLayersControl(
                baseGroups = c("Hillshade","Positron","Satellite", "OpenStreetMap"),
                overlayGroups = c("BGCs","Feasibility","Districts","Cities"),
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
            leaflet::addTiles(
                urlTemplate = paste0("https://api.mapbox.com/styles/v1/", mbsty, "/tiles/{z}/{x}/{y}?access_token=", mbtk),
                attribution = '&#169; <a href="https://www.mapbox.com/feedback/">Mapbox</a>',
                group = "Hillshade",
                options = leaflet::pathOptions(pane = "mapPane")) %>%
            leaflet::addTiles(
                urlTemplate = paste0("https://api.mapbox.com/styles/v1/", mblbsty, "/tiles/{z}/{x}/{y}?access_token=", mbtk),
                attribution = '&#169; <a href="https://www.mapbox.com/feedback/">Mapbox</a>',
                group = "Cities",
                options = leaflet::pathOptions(pane = "overlayPane")) %>%
            addSelectBEC() %>%
            leaflet::addLayersControl(
                baseGroups = c("Hillshade","Positron","Satellite", "OpenStreetMap"),
                overlayGroups = c("BEC","Cities"),
                position = "topright")
    })
    
    observeEvent(input$selectBGC,{
        if(input$selectBGC == "(N)"){
            updatePickerInput(session,"selectSubzone",choices = subzones,selected = "")
            session$sendCustomMessage("clearBEC","xxx")
        }else{
            temp <- subzones[grep(input$selectBGC,subzones)]
            updatePickerInput(session,"selectSubzone",choices = temp,selected = temp)
        }
    })
    
    observeEvent(input$selectSubzone,{
        session$sendCustomMessage("highlightBEC",input$selectSubzone)
    })
    
    observeEvent(input$becselect_click,{
        output$selectedBEC <- renderText({
            if(length(input$becselect_click) > 1){
                c("Selected BGC: ",
                  input$selectBGC)
            }else{
                c("Selected BGC: ",
                  input$becselect_click)
            }
           
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
            leaflet::addTiles(
                urlTemplate = paste0("https://api.mapbox.com/styles/v1/", mbsty, "/tiles/{z}/{x}/{y}?access_token=", mbtk),
                attribution = '&#169; <a href="https://www.mapbox.com/feedback/">Mapbox</a>',
                group = "Hillshade",
                options = leaflet::pathOptions(pane = "mapPane")) %>%
            leaflet::addTiles(
                urlTemplate = paste0("https://api.mapbox.com/styles/v1/", mblbsty, "/tiles/{z}/{x}/{y}?access_token=", mbtk),
                attribution = '&#169; <a href="https://www.mapbox.com/feedback/">Mapbox</a>',
                group = "Cities",
                options = leaflet::pathOptions(pane = "overlayPane")) %>%
            addBGCTiles() %>%
            leaflet::addLayersControl(
                baseGroups = c("Hillshade","Positron","Satellite", "OpenStreetMap"),
                overlayGroups = c("BGCs","Districts","Cities"),
                position = "topright")
    })
    
    observeEvent(input$trialSelect,{
        output$assIn <- renderRHandsontable({
            if(input$trialSelect != ""){
                dat <- dbGetQuery(con,paste0("select plotid,spp,numplanted,seedlot,assessment from offsite where plotid = '",
                                             input$trialSelect,"'"))
                dat <- unique(as.data.table(dat))
                rhandsontable(dat) %>%
                    hot_col(col = "assessment",type = "dropdown",source = c("Fail","Poor","Fair","Good","Excellent","UN"))
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
        nme <- gsub("Name: ","",val)
        updateSelectInput(session,"trialSelect",selected = nme)
    })
    
    output$addTrial <- renderRHandsontable({
        input$submitTrial
        rhandsontable(trialInit) %>%
            hot_col("assessment", type = "dropdown", 
                    source = c("Fail","Poor","Fair","Good","Excellent","UN"),strict = T)
    })
    
    observeEvent(input$offsiteMap_click,{
        lat <- input$offsiteMap_click$lat
        long <- input$offsiteMap_click$lng
        updateTextInput(session,"addTr_lat",value = lat)
        updateTextInput(session, "addTr_long", value = long)
    })
    
    observeEvent(input$submitTrial,{
        dat <- as.data.table(hot_to_r(input$addTrial))
        dat[,`:=`(plotid = input$addTr_id,planted = input$addTr_planted,
                  project_id = input$addTr_proj, mod = input$trialMod,
                  lat = input$addTr_lat,long = input$addTr_long)]
        dat <- st_as_sf(dat, coords = c("long","lat"), crs = 4326)
        st_write(dat, con, "offsite", append = TRUE)
        shinyalert("Thank you!","Your trial has been recorded", type = "info", inputId = "trialMessage")
        updateTextInput(session,"addTr_id",value = "")
        updateDateInput(session,"addTr_planted",value = as.Date("2000-01-01"))
        updateTextInput(session,"addTr_id",value = "")
    },priority = 5)
    
    observeEvent({c(input$trials2,
                    input$trialStart2,
                    input$sppPick2,
                    input$multiSppTrial)},{
                        if(!is.null(input$trials2)){
                            if(input$multiSppTrial){
                                if(input$sppPick2 == "All"){
                                    dat2 <- st_read(con,query = paste0("select project_id, plotid, spp, seedlot,assessment, geometry from offsite where project_id in ('",paste(input$trials2,collapse = "','"),
                                                                       "') and planted > '", input$trialStart2[1],"' and planted < '",input$trialStart2[2],
                                                                       "' and plotid in (select plotid from offsite group by plotid having count(distinct spp) > 1)"))
                                }else{
                                    dat2 <- st_read(con,query = paste0("select project_id, plotid, spp, seedlot,assessment, geometry from offsite where project_id in ('",paste(input$trials2,collapse = "','"),
                                                                       "') and planted > '", input$trialStart2[1],"' and planted < '",input$trialStart2[2],"' and spp like '", substr(input$sppPick2,1,2),
                                                                       "%' and plotid in (select plotid from offsite group by plotid having count(distinct spp) > 1)"))
                                }
                            }else{
                                if(input$sppPick2 == "All"){
                                    dat2 <- st_read(con,query = paste0("select project_id, plotid, spp, seedlot,assessment, geometry from offsite where project_id in ('",paste(input$trials2,collapse = "','"),
                                                                       "') and planted > '", input$trialStart2[1],"' and planted < '",input$trialStart2[2],
                                                                       "'"))
                                }else{
                                    dat2 <- st_read(con,query = paste0("select project_id, plotid, spp, seedlot,assessment, geometry from offsite where project_id in ('",paste(input$trials2,collapse = "','"),
                                                                       "') and planted > '", input$trialStart2[1],"' and planted < '",input$trialStart2[2],"' and spp like '", substr(input$sppPick2,1,2),
                                                                       "%'"))
                                }
                            }

                            if(nrow(dat2) == 0){
                                dat2 <- NULL
                            }else{
                                plotLocs <- unique(dat2["plotid"])
                                dat <- as.data.table(st_drop_geometry(dat2))
                                if(input$sppPick2 == "All"){
                                    dat <- dat[,.(Col = if(all(assessment == "UN")) "#6B6B6B" else "#AD00BD"),
                                               by = .(plotid)]
                                }else{
                                    dat[assID, ID := i.ID, on = "assessment"]
                                    dat <- dat[,.(ID = max(ID)), by = .(plotid,spp)]
                                    dat[assCols, Col := i.Col, on = "ID"]
                                }
                                dat[,label := paste0("Name: ",plotid)]
                                dat <- dat[,.(plotid,label,Col)]
                                dat[,Col := as.character(Col)]
                                updateSelectInput(session,"trialSelect",choices = unique(dat$plotid))
                                plotLocs <- merge(plotLocs, dat, by = "plotid")
                                leafletProxy("offsiteMap") %>%
                                    addGlPoints(data = plotLocs,layerId = "tree_trial",popup = ~ label,
                                                fillColor = ~ Col,fragmentShaderSource = "point")
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
        if(nrow(dat) == 0){
            updateSelectInput(session, "pestSpp", choices = "")
        }else{
            dat$pest_name <- paste0(dat$pest," - ", dat$pest_name)
            temp <- dat$pest
            names(temp) <- dat$pest_name
            pList <- list()
            for(pcat in pestCat$pest){
                pList[[pcat]] <- temp[temp %in% pestCat[pest == pcat,pest_code]]
            }
            pList[["Other"]] <- temp[!temp %in% pestCat$pest_code]
            updateSelectInput(session, "pestSpp", choices = pList)
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
            leaflet::addTiles(
                urlTemplate = paste0("https://api.mapbox.com/styles/v1/", mbsty, "/tiles/{z}/{x}/{y}?access_token=", mbtk),
                attribution = '&#169; <a href="https://www.mapbox.com/feedback/">Mapbox</a>',
                group = "Hillshade",
                options = leaflet::pathOptions(pane = "mapPane")) %>%
            leaflet::addTiles(
                urlTemplate = paste0("https://api.mapbox.com/styles/v1/", mblbsty, "/tiles/{z}/{x}/{y}?access_token=", mbtk),
                attribution = '&#169; <a href="https://www.mapbox.com/feedback/">Mapbox</a>',
                group = "Cities",
                options = leaflet::pathOptions(pane = "overlayPane")) %>%
            addBGCTiles() %>%
            leaflet::addLayersControl(
                baseGroups = c("Hillshade","Positron","Satellite", "OpenStreetMap"),
                overlayGroups = c("BGCs","Pests","Districts","Cities"),
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
        if(nrow(d1) != 0){
            feas <- as.data.table(d1)
            feasMax <- feas[,.(SuitMax = min(feasible)), by = .(bgc,spp)]
            feasMax[,Col := "#443e3dFF"]
            feasMax[,Lab := bgc]
            feasMax[,.(bgc,Col,Lab)]
        }else{
            data.table()
        }
        
    })
    
    observeEvent({c(input$pestSpp,input$submitFH,input$submitFHLong)},{
        dat <- dbGetQuery(con,paste0("select bgc,hazard_update from forhealth where treecode like '",
                                     substr(input$fhSpp,1,2),"' and pest = '",input$pestSpp,
                                     "' and hazard_update <> 'UN'"))
        dat <- as.data.table(dat)
        #browser()
        if(nrow(dat) > 0){
            setnames(dat, old = "hazard_update", new = "hazard")
            dat[fhCols,fhcol := i.Col, on = "hazard"]
            rangeDat <- prepDatFH()
            dat[rangeDat, InRange := i.Col, on = "bgc"]
            dat[is.na(InRange), fhcol := "#840090"]
            session$sendCustomMessage("colourPest",dat[,.(bgc,fhcol)])
        }
        
    })
    
    observeEvent({c(
        input$fhSpp)
    },{
        dat <- prepDatFH()
        if(nrow(dat) == 0){
            dat <- NULL
        }else{
            print("Rendering map")
            dat <- dat[subzTransparent, on = "bgc"]
            dat[is.na(Col),Col := Transparent]
            dat[is.na(Lab),Lab := bgc]
            if(!is.null(dat)){
                leafletProxy("fhMap") %>%
                    invokeMethod(data = dat, method = "addFHTiles", dat$bgc, dat$Col,dat$Lab)
            }
        }
        
        
    },priority = 10)
    
    observeEvent(input$fh_click,{
        output$pestDatLabel <- renderText({
            paste0("Pest Table for ",input$fh_click)
        })
    })
    
    observeEvent({c(input$fh_click,input$fhSpp,input$pestSpp,input$submitFHLong)},{
                dat1 <- dbGetQuery(con,paste0("select treecode, pest, pest_name, bgc, hazard, hazard_update 
                                          from forhealth where bgc = '",input$fh_click,"'"))
                dat <- as.data.table(dat1)
                if(nrow(dat) < 1){
                    dat <- data.table()
                    col_num <- NULL
                    row_num <- NULL
                }else{
                    dat <- dcast(dat, pest_name + pest ~ treecode, value.var = "hazard_update", 
                                 fun.aggregate = function(x) x[1])
                    dat[is.na(dat)] <- "NULL"
                    col_num <- which(colnames(dat) == substr(input$fhSpp,1,2)) - 1
                    row_num <- which(dat$pest == input$pestSpp) - 1
                }
                output$fh_hot <- renderRHandsontable({
                    rhandsontable(dat,col_highlight = col_num, 
                                  row_highlight = row_num) %>%
                        hot_cols(type = "dropdown",source = c("Nil","Low","Moderate","High","UN"),
                                 renderer = 
                                     "function(instance, td, row, col, prop, value, cellProperties) {
                                        Handsontable.renderers.TextRenderer.apply(this, arguments);
                                      if(instance.params) {
                                        hcols = instance.params.col_highlight
                                        hcols = hcols instanceof Array ? hcols : [hcols]
                                        hrows = instance.params.row_highlight
                                        hrows = hrows instanceof Array ? hrows : [hrows]
                                      }
                                      if(value == 'NULL') { 
                                            td.style.background = 'lightgrey'; 
                                            td.style.color = 'lightgrey';
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
    })
    
    observeEvent({c(input$pestSpp,
                    input$fhSpp)},{
        dat <- dbGetQuery(con,paste0("select distinct bgc, hazard, hazard_update from forhealth where treecode = '",
                          substr(input$fhSpp,1,2),"' and pest = '",input$pestSpp,"'"))
        if(nrow(dat) > 0){
            output$fh_hot_long <- renderRHandsontable({
                rhandsontable(dat) %>%
                    hot_col("hazard_update",type = "dropdown",source = c("Nil","Low","Moderate","High","UN"),strict = T)
            })
        }
    
    })

    observeEvent(input$submitFH,{
        dat <- as.data.table(hot_to_r(input$fh_hot))
        dat[dat == "NULL"] <- NA
        dat <- melt(dat,id.vars = c("pest_name","pest"),variable.name = "treecode",value.name = "hazard_update")
        dat[,mod := input$fhMod]
        dat[,bgc := input$fh_click]
        if(nrow(dat) > 0){
            currPests <- dbGetQuery(con,"select distinct pest from forhealth")[,1]
            if(any(!dat$pest %in% currPests)){
                d2 <- dat[!pest %chin% currPests,]
                d2[,hazard := NA]
                d2 <- d2[!is.na(hazard_update),]
                dbWriteTable(con,"forhealth",d2, append = T, row.names = F)
                dat <- dat[pest %chin% currPests,]
            }
            dbWriteTable(con, "temp_fh", dat, overwrite = T,row.names = F)
            dbExecute(con,"UPDATE forhealth
                  SET hazard_update = temp_fh.hazard_update,
                  mod = temp_fh.mod
                  FROM temp_fh
                  WHERE forhealth.treecode = temp_fh.treecode
                  AND forhealth.pest = temp_fh.pest
                  AND forhealth.bgc = temp_fh.bgc")
        }
        shinyalert("Thank you!","Your updates have been recorded", type = "info",
                   imageUrl = "images/puppy1.jpg",imageHeight = "100px",inputId = "fhMessage")
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
        shinyalert("Thank you!","Your updates have been recorded", type = "info",
                   imageUrl = "images/puppy1.jpg",imageHeight = "100px", inputId = "fhMessage")
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
                            if(nrow(dat) > 0){
                                dat <- dat["plotnum"]
                                colnames(dat)[1] <- "label"
                                leafletProxy("map") %>%
                                    addGlPoints(data = dat,layerId = "tree_plot",popup = ~ label,
                                                fillColor = "#2DB000",fragmentShaderSource = "point")
                            }
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
                                leafletProxy("map") %>%
                                    removeGlPoints("tree_trial")
                            }else{
                                dat <- as.data.table(st_drop_geometry(dat2))
                                dat[assID, ID := i.ID, on = "assessment"]
                                dat <- dat[,.(ID = max(ID)), by = .(plotid,spp)]
                                dat[assCols, Col := i.Col, on = "ID"]
                                dat[,label := paste0("Name: ",plotid)]
                                dat <- dat[,.(plotid,label,Col)]
                                dat[,Col := as.character(Col)]
                                plotLocs <- merge(dat2, dat, by = "plotid")
                                
                                leafletProxy("map") %>%
                                    addGlPoints(data = plotLocs,layerId = "tree_trial",popup = ~ label,
                                                fillColor = plotLocs$Col,fragmentShaderSource = "square")
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
        if(input$showFreq){
            feasMax <- prepFreq()
            sppOpts <- unique(feasMax$sppsplit)
            feasMax[,sppsplit := as.numeric(as.factor(sppsplit))]
            feasMax[taxaFreqCols, Col := i.Col, on = c("sppsplit","Freq")]
            feasMax[Freq == "Added",Col := "#fbff00ff"]
            feasMax[Freq == "Removed",Col := "#8300ffff"]
            PALeg <- list(
                labels = c(sppOpts,"Added","Removed"),
                colours = c(taxaCols[1:length(sppOpts)],"#fbff00ff","#8300ffff"),
                title = "Presence/Absence"
            )
            globalLeg$Legend <- PALeg
        }else{
            if(length(unique(feasMax$sppsplit)) > 1){
                temp <- unique(feasMax$sppsplit)
                tempTab <- data.table(sppsplit = temp, Col = taxaCols[1:length(temp)])
                feasMax[tempTab,Col := i.Col, on = "sppsplit"]
                temp <- unique(feasMax[,.(sppsplit,Col)])
                
                PALeg <- list(
                    labels = c(tempTab$sppsplit,"Added","Removed"),
                    colours = c(tempTab$Col,"#fbff00ff","#8300ffff"),
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
        }
        feasMax[,Lab := bgc]
        feasMax[,.(bgc,Col,Lab)]
    })
    
    prepFreq <- reactive({
        QRY <- paste0("select bgc,ss_nospace,sppsplit,spp,",globalFeas$dat,
                        " from feasorig where spp = '",substr(input$sppPick,1,2),
                        "' and ",globalFeas$dat," in (1,2,3,4,5)")
        feas <- as.data.table(dbGetQuery(con, QRY))
        setnames(feas, old = globalFeas$dat, new = "feasible")
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
        allFreq
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
        if(nrow(feasSub) == 0){
            return(NULL)
        }
        feasSub[,Lab := paste0(ss_nospace,": ", feasible)]
        feasSum <- feasSub[,.(FeasVal = mean(feasible), Lab = paste(Lab, collapse = "<br>")), by = bgc]
        #tempCol <- grRamp(rescale(feasSum$FeasVal,to = c(0,1)))
        feasSum[,Col := colour_values(rescale(feasSum$FeasVal,to = c(0,1)))]
        feasSum[,.(bgc,Col,Lab)]
    })
    
    
    observeEvent({c(
        input$sppPick,
        input$edaplot_selected,
        input$updatedfeas,
        input$showFreq)
    },{
        if(input$sppPick == "None"){
            session$sendCustomMessage("clearLayer","xxx")
        }else{
            if(is.null(input$edaplot_selected)){
                dat <- prepDatSimple()
            }else{
                dat <- prepEdaDat()
            }
            if(is.null(dat)){
                dat <- NULL
                session$sendCustomMessage("clearLayer", "Delete")
            }else{
                print("Rendering map")
                dat <- dat[subzTransparent, on = "bgc"]
                dat[is.na(Col),Col := Transparent]
                dat[is.na(Lab),Lab := bgc]
                leafletProxy("map") %>%
                    invokeMethod(data = dat, method = "addGridTiles", ~bgc, ~Col, ~Lab) %>%
                    addLegend(position = "bottomright",
                              labels = globalLeg$Legend$labels,
                              colors = globalLeg$Legend$colours,
                              title = globalLeg$Legend$title,
                              layerId = "bec_feas") 
            }
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
            # if(input$type == "Climatic Suitability"){
            #     tempFeas <- feas[feasible %in% c(1,2,3),]
            #     tempEda <- eda[tempFeas, on = "ss_nospace"]
            #     tempEda <- tempEda[!is.na(smr),]
            #     tempEda <- tempEda[,.(AvgSMR = mean(smr)), by = .(ss_nospace,feasible,sppsplit)]
            #     tempEda[,SSType := fifelse(grepl("01",ss_nospace),"Zonal",fifelse(AvgSMR <= 3.5,"Dry",
            #                                                                       fifelse(AvgSMR > 4.1,"Wet","??")))]
            #     tabOut <- dcast(tempEda, SSType ~ sppsplit, value.var = "feasible", fun.aggregate = min)
            #     tabOut[tabOut == 0] <- NA
            # }else{
                feasSub <- feas[sppsplit != "X",]
                tabOut <- data.table::dcast(feasSub, ss_nospace ~ sppsplit,fun.aggregate = mean, value.var = "feasible")
                tabOut[,lapply(.SD,as.integer),.SDcols = -"ss_nospace"]
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
        shinyalert("Enter your initials:", type = "input",imageUrl = "images/puppy1.jpg",imageHeight = "100px", inputId = "initials", callbackR = sendToDb)
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
        shinyalert("Thank you!","Your updates have been recorded", type = "info",
                   imageUrl = "images/puppy1.jpg",imageHeight = "100px", inputId = "dbmessage")

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
            shinyalert("Thank you!","Your updates have been recorded", type = "info",
                       imageUrl = "images/puppy1.jpg",imageHeight = "100px",inputId = "dbmessage")
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
                "Maximum feasibility by subzone"
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

# ###calculate climatic suitability colours
# prepClimSuit <- reactive({
#     QRY <- paste0("select bgc,ss_nospace,sppsplit,spp,",globalFeas$dat,
#                   " from feasorig where spp = '",substr(input$sppPick,1,2),
#                   "' and ",globalFeas$dat," in (1,2,3,4,5)")
#     feas <- as.data.table(dbGetQuery(con, QRY))
#     setnames(feas, old = globalFeas$dat, new = "feasible")
#     tempFeas <- feas[feasible %in% c(1,2,3),]
#     minDist <- tempFeas[,.SD[feasible == min(feasible, na.rm = T)],by = .(bgc)]
#     abUnits <- minDist[grep("[[:alpha:]] */[[:alpha:]]+$",ss_nospace),]
#     noAb <- minDist[!grepl("[[:alpha:]] */[[:alpha:]]+$",ss_nospace),]
#     abUnits <- eda[abUnits, on = "ss_nospace"] ##merge
#     abUnits <- abUnits[,.(Temp = if(any(grepl("C4",edatopic))) paste0(ss_nospace,"_01") else ss_nospace, feasible = feasible[1]),
#                        by = .(bgc,ss_nospace,sppsplit,spp)]
#     abUnits[,ss_nospace := NULL]
#     setnames(abUnits,old = "Temp",new = "ss_nospace")
#     minDist <- rbind(noAb,abUnits)
#     minDist[,ID := if(any(grepl("01", ss_nospace)) & feasible[1] == 1) T else F, by = .(bgc)]
#     green <- minDist[(ID),]
#     green <- green[,.(Col = zonalOpt), by = .(bgc)]
#     
#     minDist <- minDist[ID == F,]
#     minDist[,ID := if(any(grepl("01", ss_nospace))) T else F, by = .(bgc)]
#     blue <- minDist[(ID),]
#     blue <- blue[,.(feasible = min(feasible)), by = .(bgc)]
#     
#     minDist <- minDist[ID == F,]
#     minEda <- eda[minDist, on = "ss_nospace"]
#     minEda <- minEda[,.(AvgEda = mean(smr)), by = .(bgc,ss_nospace,feasible)]
#     minEda <- minEda[,.(Col = fifelse(all(AvgEda >= 3.5),"WET",
#                                       fifelse(all(AvgEda < 3.5), "DRY", splitOpt)), feasible = min(feasible)), by = .(bgc)]
#     temp <- minEda[Col == "DRY",]
#     temp[,Col := NULL]
#     blue <- rbind(blue,temp)
#     red <- minEda[Col == "WET",]
#     minEda <- minEda[!Col %in% c("WET","DRY"),.(bgc,Col)]
#     blue[dryOpt, Col := i.Col, on = "feasible"]
#     red[wetOpt, Col := i.Col, on = "feasible"]
#     blue[,feasible := NULL]
#     red[,feasible := NULL]
#     climSuit <- rbind(green,blue,red,minEda)
#     climSuit <- climSuit[!is.na(bgc),]
#     
#     tf2 <- feas[feasible %in% c(4,5),.(SuitMax = min(feasible)), by = .(bgc)]
#     if(nrow(tf2) > 0){
#         tf2[SuitMax == 4,Col := "#fbff00ff"]
#         tf2[SuitMax == 5,Col := "#8300ffff"]
#         tf2 <- tf2[,.(bgc,Col)]
#         climSuit <- rbind(climSuit, tf2)
#     }
#     return(climSuit)
# })
