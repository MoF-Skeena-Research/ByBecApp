##Kiri Daust
### start for health tab ###
observeEvent(input$showinstr_forhealth,{
  shinyalert(title = "Instructions",html = T,text = instr_forhealth)
})

observeEvent(input$fhSpp,{
  treeSpp <- substr(input$fhSpp,1,2)
  dat <- dbGetQuery(sppDb,paste0("select distinct pest,pest_name from forhealth where treecode like '",treeSpp,"'"))
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
  d1 <- dbGetQuery(sppDb, QRY)
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


observeEvent({c(input$pestSpp,input$submitFH,input$submitFHLong,input$fh_region)},{
  if(input$fh_region == "BC"){
    print("Getting BC")
    q1 <- paste0("select bgc,hazard_update from forhealth where treecode like '",
                 substr(input$fhSpp,1,2),"' and pest = '",input$pestSpp,
                 "' and hazard_update <> 'UN' and region = 'BC'")
    dat <- dbGetQuery(sppDb,q1)
  }else{
    print("getting WNA")
    q1 <- paste0("select bgc,hazard_update from forhealth where treecode like '",
                 substr(input$fhSpp,1,2),"' and pest = '",input$pestSpp,
                 "' and hazard_update <> 'UN'")
    dat <- dbGetQuery(sppDb,q1)
  }
  dat <- as.data.table(dat)
  #browser()
  if(nrow(dat) > 0){
    setnames(dat, old = "hazard_update", new = "hazard")
    dat[fhCols,fhcol := i.Col, on = "hazard"]
    rangeDat <- prepDatFH()
    dat[rangeDat, InRange := i.Col, on = "bgc"]
    dat[is.na(InRange), fhcol := "#840090"]
    session$sendCustomMessage("colourPest",dat[,.(bgc,fhcol)])
  }else{
    session$sendCustomMessage("clearPest","puppy")
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

observeEvent({c(input$fh_click,input$fhSpp,input$pestSpp,input$submitFHLong,input$fh_region)},{
  if(input$fh_region == "BC"){
    q1 <- paste0("select treecode, pest, pest_name, bgc, hazard, hazard_update 
                                          from forhealth where bgc = '",input$fh_click,"' and region = 'BC'")
  }else{
    q1 <- paste0("select treecode, pest, pest_name, bgc, hazard, hazard_update 
                                          from forhealth where bgc = '",input$fh_click,"'")
  }
  dat1 <- dbGetQuery(sppDb,q1)
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
                input$fhSpp,
                input$fh_region)},{
                  if(input$fh_region == "BC"){
                    q1 <- paste0("select distinct bgc, hazard, hazard_update from forhealth where treecode = '",
                                 substr(input$fhSpp,1,2),"' and pest = '",input$pestSpp,"' and region = 'BC'")
                  }else{
                    q1 <- paste0("select distinct bgc, hazard, hazard_update from forhealth where treecode = '",
                                 substr(input$fhSpp,1,2),"' and pest = '",input$pestSpp,"'")
                  }
                  dat <- dbGetQuery(sppDb,q1)
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
    currPests <- dbGetQuery(sppDb,"select distinct pest from forhealth")[,1]
    if(any(!dat$pest %in% currPests)){
      d2 <- dat[!pest %chin% currPests,]
      d2[,hazard := NA]
      d2 <- d2[!is.na(hazard_update),]
      dbWriteTable(sppDb,"forhealth",d2, append = T, row.names = F)
      dat <- dat[pest %chin% currPests,]
    }
    dbWriteTable(sppDb, "temp_fh", dat, overwrite = T,row.names = F)
    dbExecute(sppDb,"UPDATE forhealth
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
    dbWriteTable(sppDb, "temp_fh", dat, overwrite = T,row.names = F)
    dbExecute(sppDb,"UPDATE forhealth
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

observeEvent(input$showMatrix,{
  shinyalert(html = T,
             text = tagList(
               radioButtons("matrixType",
                            label = "Show matrix of:",
                            choices = c("Conifer","Deciduous"),
                            selected = "Conifer",
                            inline = T), 
               br(),
               br(),
               panel(style = "overflow-y:scroll; max-height: 400px; position:relative; align: centre",
                     rHandsontableOutput("hot_pestMat") 
               ),
               
             ),
             showConfirmButton = T,
             size = 'l')
})

output$hot_pestMat <- renderRHandsontable({
  if(input$matrixType == "Conifer"){
    dat <- fread("./inputs/Pest_Host_Conifer.csv")
  }else{
    dat <- fread("./inputs/Pest_Host_Decid.csv")
  }
  rhandsontable(dat) %>%
    hot_cols(renderer = 
               "function(instance, td, row, col, prop, value, cellProperties) {
                                                    Handsontable.renderers.TextRenderer.apply(this, arguments);
                                                  if(value == 1) { 
                                                        td.style.background = '#ff3936'; 
                                                    }
                                                    if(value == 2){
                                                        td.style.background = '#ff6836'; 
                                                    }
                                                    if(value == 3){
                                                        td.style.background = '#ffd036'; 
                                                    }
                                                }")
})
##end for health tab###