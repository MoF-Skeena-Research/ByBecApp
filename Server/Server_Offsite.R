##Kiri Daust

##offsite trials####
observeEvent(input$showinstr_offsite,{
  shinyalert(title = "Instructions",html = T,text = instr_offsite)
})



observeEvent(input$tabs,{
  if(input$tabs == "tab2"){
    #proj_names <- dbGetQuery(sppDb, "select distinct trial_type from offsite_site")[,1]
    updatePickerInput(session, "trialType", choices = c(proj_names,"GOM"), selected = "GOM")
    updatePickerInput(session, "sppPick2", choices = sppList2,selected = sppList2[[1]][5])
    
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
        addDrawToolbar(
          targetGroup='draw',
          editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions())) %>%
        leaflet::addLayersControl(
          baseGroups = c("Hillshade","Positron","Satellite", "OpenStreetMap"),
          overlayGroups = c("BGCs","Districts","Cities"),
          position = "topright")
    })
    
    getoffsiteData_all <- reactive({
      ##browser()
      dat <- st_read(sppDb, query = paste0("select * from offsite_site
                                      inner join offsite_planting USING (trial_id)
                                      where trial_type in ('",paste(input$trialType,collapse = "','"),
                     "') and plantingyear > '", input$trialStart2[1],"' and plantingyear < '",input$trialStart2[2],
                     "' and spp in ('", paste(substr(input$sppPick2,1,2),collapse = "','"),
                     "')"))
      coords <- as.data.table(st_coordinates(dat))
      dat <- as.data.table(st_drop_geometry(dat))
      setnames(coords, c("Lat","Long"))
      dat2 <- cbind(coords,dat)
      #browser()
      return(dat2)
    })
    
    getoffsiteData <- reactive({
      if(!is.null(input$offsiteMap_draw_new_feature)){
        feature <- input$offsiteMap_draw_new_feature
        temp <- feature$geometry$coordinates[[1]]
        coordMat <- matrix(unlist(temp),ncol = 2,byrow = T)
        feat <- st_as_sf(as.data.frame(coordMat),coords = c(1,2),crs = 4326)
        feat <- st_cast(st_combine(feat),"POLYGON")
        
        sql <- paste0(
          "with poly as (
        SELECT ST_PolygonFromText('", st_as_text(feat), "',4326) geometry
        )
  
        select * from offsite_site
        join poly
        on ST_Within(offsite_site.geometry,poly.geometry)
        inner join offsite_planting USING (trial_id)
        "
        )
        dat <- st_read(sppDb,query = sql)
        #browser()
        coords <- as.data.table(st_coordinates(dat))
        dat <- as.data.table(st_drop_geometry(dat))
        setnames(coords, c("Lat","Long"))
        dat2 <- cbind(coords,dat)
        dat2[,`geometry..24` := NULL]
        #browser()
        return(dat2)
      }else{
        shinyalert("No data selected!!")
      }
      
    })
    
    output$download_offsite <- downloadHandler(
      filename = "Offsite_Data_Select.csv",
      content = function(file){
        fwrite(getoffsiteData(),file)
      }
    )
      
    output$download_offsite_all <- downloadHandler(
      filename = "Offsite_Data_All.csv",
      content = function(file){
        fwrite(getoffsiteData_all(),file)
      }
    ) 


    
    # observeEvent(input$trialSelect,{
    #   output$assIn <- renderRHandsontable({
    #     if(input$trialSelect != ""){
    #       dat <- dbGetQuery(sppDb,paste0("select plotid,spp,numplanted,seedlot,assessment from offsite where plotid = '",
    #                                      input$trialSelect,"'"))
    #       dat <- unique(as.data.table(dat))
    #       rhandsontable(dat) %>%
    #         hot_col(col = "assessment",type = "dropdown",source = c("Fail","Poor","Fair","Good","Excellent","UN"))
    #     }
    #   })
    # })
  }
})

observeEvent(input$goToLocation,{
  if(!is.null(input$lat) & !is.null(input$long)){
    leafletProxy("offsiteMap") %>%
      flyTo(lat = input$lat, lng = input$long, zoom = 12)
  }
})

observeEvent(input$addoffsite,{
  globalLatLong$lat <- NA_real_
  globalLatLong$long <- NA_real_
  showModal(modalDialog(
    actionButton("usemap","Select location on Map?"),
    actionButton("usemapno","Enter location Manually")
  ))
})

observeEvent(input$offsiteMap_click,{
  if(globalLatLong$useMap){
    globalLatLong$lat <- input$offsiteMap_click$lat
    globalLatLong$long <- input$offsiteMap_click$lng
    globalLatLong$useMap <- F
    showModal(modalDialog(
      h2("Trial ID"),
      textInput("newtrial_id", "Enter Trial ID"),
      h2("Site Info"),
      rHandsontableOutput("blanksite"),
      h2("Planting Info"),
      rHandsontableOutput("blankplant"),
      actionButton("submitoffsite_site","Submit to Database")    ))
    
  }
  
})

observeEvent(input$usemapno,{
  closeAlert()
  showModal(modalDialog(
    h2("Trial ID"),
    textInput("newtrial_id", "Enter Trial ID"),
    h2("Site Info"),
    rHandsontableOutput("blanksite"),
    h2("Planting Info"),
    rHandsontableOutput("blankplant"),
    actionButton("submitoffsite_site","Submit to Database"),
    size = "xl"
  ))
})


observeEvent(input$usemap,{
  #browser()
  #closeAlert()
  removeModal()
  globalLatLong$useMap <- T
})

# observeEvent(input$addSite,{
#   closeAlert()
#   showModal(modalDialog(
#                h2("Trial ID"),
#                textInput("newtrial_id", "Enter Trial ID"),
#                h2("Site Info"),
#                rHandsontableOutput("blanksite"),
#                h2("Planting Info"),
#                rHandsontableOutput("blankplant"),
#                actionButton("submitoffsite_site","Submit to Database"),
#                size = "xl"
#              ))
# })

observeEvent(input$submitoffsite_site,{
  
  site <- as.data.table(hot_to_r(input$blanksite))
  site[,trial_id := input$newtrial_id]
  site <- as.data.table(st_as_sf(site, coords = c("Long","Lat"), 
                   crs = 4326, agr = "constant"))
  site2 <- rbind(sitenames, site, fill = T)
  site2 <- st_as_sf(site2[-1,])
  st_write(site2, sppDb,"offsite_site",append = T, row.names = F)
  plant <- as.data.table(hot_to_r(input$blankplant))
  plant <- plant[!is.na(spp) | !is.na(sppvar),]
  if(nrow(plant) > 0){
    plant[,trial_id := input$newtrial_id]
    plant[,spp := substr(sppvar,1,2)]
    plant2 <- rbind(plantingnames,plant, fill =  T)
    dbAppendTable(sppDb,"offsite_planting",plant2)
  }
  globalLatLong$lat <- NA_real_
  globalLatLong$long <- NA_real_
  removeModal()
})

observeEvent(input$addPlanting,{
  closeAlert()
  # updateSelectizeInput(session,"trialaddSelect",choices = offsiteTrials,
  #                      server = TRUE)
  showModal(modalDialog(
               h2("Select Site"),
               selectInput("trialaddSelect",
                           label = "Select a trial",
                           choices = offsiteTrials),
               h2("Planting Table"),
               rHandsontableOutput("newplanting"),
               actionButton("submitoffsite_planting","Submit to Database"),
               size = "xl"
             ))
})

observeEvent(input$submitoffsite_planting,{
  datOrig <- setDT(dbGetQuery(sppDb,paste0("select * from offsite_planting
                                         where trial_id = '",input$trialaddSelect,"'")))
  plant <- as.data.table(hot_to_r(input$newplanting))
  if(nrow(datOrig) > 0){
    plant <- plant[-c(1:nrow(datOrig)),]
    plant[,trial_id := input$trialaddSelect]
    plant[,spp := substr(sppvar,1,2)]
    plant2 <- rbind(plantingnames,plant, fill =  T)
    dbAppendTable(sppDb,"offsite_planting",plant2)
  }
  removeModal()
})


observeEvent(input$trialaddSelect,{
  output$newplanting <- renderRHandsontable({
    if(!is.null(input$trialaddSelect)){
      dat <- setDT(dbGetQuery(sppDb,paste0("select * from offsite_planting
                                         where trial_id = '",input$trialaddSelect,"'")))
      print(dat)
      rhandsontable(dat) %>%
        hot_table(minSpareRows = 1)
    }
  })
})
##


  output$blanksite <- renderRHandsontable({
    dat <- setDT(dbGetQuery(sppDb,"select * from offsite_site limit 1"))
    dat[,geometry := NULL]
    dat[,trial_id := NULL]
    dat[1,] <- NA
    print(str(dat))
    datTemp <- data.table(Lat = globalLatLong$lat,Long = globalLatLong$long)
    dat <- cbind(datTemp, dat)
    rhandsontable(dat,overflow = "hidden",height = 200) %>%
      hot_cols(format = "0.00000") %>%
      hot_col("trial_type",type = "dropdown",
              source = c("Research","Operational","Other"),
              strict = T)
  })
  
  output$blankplant <- renderRHandsontable({
    dat <- setDT(dbGetQuery(sppDb,"select * from offsite_planting limit 0"))
    dat[,trial_id := NULL]
    print(str(dat))
    rhandsontable(dat,overflow = "hidden",height = 200) %>%
      hot_table(minSpareRows = 1) %>%
      hot_col("sppvar", type = "dropdown",source = sppData, strict = F) %>%
      hot_col("spp", type = "dropdown",source = sppData, strict = F) %>%
      hot_col("qualitative_vigour", type = "dropdown",
              source = c("Excellent", "Good", "Fair", "Poor", "Fail", "UN"),strict = T)
  })

# observeEvent(input$addoffsite,{
#   shinyalert("Select location or enter manually?",)
# })
  
  observeEvent(input$completeOffsite,{
    #browser()
    output$offsite_planting_full <- renderRHandsontable({
      dat <- setDT(dbGetQuery(sppDb,paste0("select *
                                           from offsite_planting where trial_id = '", 
                                           globalTrialID$ID,"'")))
      rhandsontable(dat, overflow = "hidden", height = 200) %>%
        hot_col("qualitative_vigour", type = "dropdown", 
                source = c("Excellent", "Good", "Fair", "Poor", "Fail", "UN"),strict = T) %>%
        hot_col("spp",type = "dropdown",
                source = sppData, strict = F) %>%
        hot_col("sppvar",type = "dropdown",
                source = sppData, strict = F)
    })
    
    output$offsite_site_full <- renderRHandsontable({
      dat <- setDT(dbGetQuery(sppDb,paste0("select * 
                                           from offsite_site where trial_id = '",
                                           globalTrialID$ID,"'")))
      dat[,geometry := NULL]
      rhandsontable(dat, height = 200)
    })
    
    showModal(modalDialog(
      h2("Site Info"),
      rHandsontableOutput("offsite_site_full"),
      h2("Planting Info"),
      rHandsontableOutput("offsite_planting_full"),
      actionButton("submitAss_full","Submit Updates")
   ))
  })

  

  output$offsite_site <- renderRHandsontable({
    if(!is.null(globalTrialID$ID)){
      if(("GOM" %in% input$trialType) & nchar(globalTrialID$ID) > 30){
        dat <- setDT(dbGetQuery(gomDb,paste0("select plantation_id,date_established,
                                           site_series,elevation 
                                           from planting_info where trial_id = '",
                                             globalTrialID$ID,"'"))) %>% unique()
        rhandsontable(dat,colHeaders = c("Plantation_ID","Established","BGC",
                                         "Elevation"))
      }else{
        dat <- setDT(dbGetQuery(sppDb,paste0("select trial_id,project_name,trial_type,
                                           plantingyear,bgc,ss_nospace,elevation 
                                           from offsite_site where trial_id = '",
                                             globalTrialID$ID,"'")))
        rhandsontable(dat,colHeaders = c("Trial_ID","Project","Trial","Planting Year","BGC",
                                         "Site Series","Elevation"))
      }
        
        
      }
      
  })
  
  

  output$offsite_planting <- renderRHandsontable({
    if(!is.null(globalTrialID$ID)){
      if(("GOM" %in% input$trialType) & nchar(globalTrialID$ID) > 30){
        dat <- setDT(dbGetQuery(gomDb,paste0("select plantation_id, species, seedlot,trees_number
                                           from planting_info where trial_id = '", 
                                             globalTrialID$ID,"'")))
        rhandsontable(dat,colHeaders = c("Plantation_ID","Spp","Seedlots","Number Planted"),overflow = "visible")
      }else{
        dat <- setDT(dbGetQuery(sppDb,paste0("select trial_id, spp, seedlots,num_planted, qualitative_vigour, assessor_name_qual, qual_date
                                           from offsite_planting where trial_id = '", 
                                             globalTrialID$ID,"'")))
        #dat[,qualitative_vigour := factor(qualitative_vigour,levels = c("Excellent", "Good", "Fair", "Poor", "Fail", "UN"))]
        rhandsontable(dat,colHeaders = c("Trial_ID","Spp","Seedlots","Number Planted","Vigour", "Assessor","Assess Date"),overflow = "visible") %>%
          hot_col("Vigour", type = "dropdown", 
                  source = c("Excellent", "Good", "Fair", "Poor", "Fail", "UN"),strict = T) %>%
          hot_col("Spp",type = "dropdown",
                  source = sppData, strict = F)
      }
        
      
    }
  })


observeEvent(input$submitAss,{
    dat <- as.data.table(hot_to_r(input$offsite_planting))
    ##dat[,mod := input$assessMod]
    #print(dat)
    datOrig <- setDT(dbGetQuery(sppDb,paste0("select trial_id as orig_id, spp, seedlots
                                           from offsite_planting where trial_id = '", 
                                             globalTrialID$ID,"'")))
    if(nrow(dat) > nrow(datOrig)){##added new line
      dat <- merge(dat, datOrig, by = c("spp","seedlots"),all = T)
      dat <- dat[is.na(orig_id),]
      dat[,orig_id := NULL]
      dat[,trial_id := globalTrialID$ID]
      dat <- rbind(plantingnames,dat, fill =  T)
      dbAppendTable(sppDb,"offsite_planting",dat)
    }else{
      dbWriteTable(sppDb, "temp_ass_update", dat, overwrite = T)
      dbExecute(sppDb,"UPDATE offsite_planting
                  SET qualitative_vigour = temp_ass_update.qualitative_vigour,
                  assessor_name_qual = temp_ass_update.assessor_name_qual,
                  qual_date = temp_ass_update.qual_date
                  FROM temp_ass_update
                  WHERE offsite_planting.trial_id = temp_ass_update.trial_id
                  AND offsite_planting.spp = temp_ass_update.spp
                  AND offsite_planting.seedlots = temp_ass_update.seedlots")
    }
    
    shinyalert("Thank you!","Your updates have been recorded", type = "info", inputId = "assMessage")
  
})

observeEvent(input$submitAss_full,{
  req(input$submitAss_full)
  dat2 <- hot_to_r(input$offsite_planting_full)
  if(!is.null(dat2)) {
    dat <- as.data.table(dat2)
    removeModal()
  }
  ##dat[,mod := input$assessMod]
  #print(dat)
  datOrig <- setDT(dbGetQuery(sppDb,paste0("select trial_id as orig_id, spp, seedlots
                                           from offsite_planting where trial_id = '",
                                           globalTrialID$ID,"'")))
  if(nrow(dat) > nrow(datOrig)){##added new line
    dat <- merge(dat, datOrig, by = c("spp","seedlots"),all = T)
    dat <- dat[is.na(orig_id),]
    dat[,orig_id := NULL]
    dat[,trial_id := globalTrialID$ID]
    dat <- rbind(plantingnames,dat, fill =  T)
    dbAppendTable(sppDb,"offsite_planting",dat)
  }else{
    dbWriteTable(sppDb, "temp_ass_update", dat, overwrite = T)
    dbExecute(sppDb,"UPDATE offsite_planting
                  SET qualitative_vigour = temp_ass_update.qualitative_vigour,
                  assessor_name_qual = temp_ass_update.assessor_name_qual,
                  qual_date = temp_ass_update.qual_date
                  FROM temp_ass_update
                  WHERE offsite_planting.trial_id = temp_ass_update.trial_id
                  AND offsite_planting.spp = temp_ass_update.spp
                  AND offsite_planting.seedlots = temp_ass_update.seedlots")
  }

  shinyalert("Thank you!","Your updates have been recorded", type = "info", inputId = "assMessage")

})

observeEvent(input$offsiteMap_marker_click,{
  val <- input$offsiteMap_marker_click
  globalTrialID$ID = val$id
})

observeEvent({c(input$trialType,
                input$trialStart2,
                input$sppPick2,
                input$multiSppTrial,
                input$submitAss,
                #input$submitAss_full,
                input$submitoffsite_site)},{
                  leafletProxy("offsiteMap") %>%
                    clearMarkers()
                  if(!is.null(input$trialType)){
                    #browser()
                    if(input$multiSppTrial){
                      if(input$trialStart2[1] == minStart & input$trialStart2[2] == maxStart){
                        dat2 <- st_read(sppDb,query = paste0("select offsite_planting.trial_id, spp, trial_type, qualitative_vigour, geometry 
                                                             from offsite_planting 
                                                             join offsite_site using (trial_id) 
                                                             where trial_type in ('",paste(input$trialType,collapse = "','"),
                                                             "') and spp in ('", paste(substr(input$sppPick2,1,2),collapse = "','"),
                                                             "') and trial_id in (select trial_id from offsite_planting group by trial_id having count(distinct spp) > 1)"))
                      }else{
                        dat2 <- st_read(sppDb,query = paste0("select offsite_planting.trial_id, spp, trial_type, qualitative_vigour, geometry 
                                                             from offsite_planting 
                                                             join offsite_site using (trial_id) 
                                                             where trial_type in ('",paste(input$trialType,collapse = "','"),
                                                             "') and plantingyear > '", input$trialStart2[1],"' and plantingyear < '",input$trialStart2[2],
                                                             "' and spp in ('", paste(substr(input$sppPick2,1,2),collapse = "','"),
                                                             "') and trial_id in (select trial_id from offsite_planting group by trial_id having count(distinct spp) > 1)"))
                      }
                    }else{ ### need to join both to get trial type, 
                      if(input$trialStart2[1] == minStart & input$trialStart2[2] == maxStart){
                        dat2 <- st_read(sppDb,query = paste0("select offsite_planting.trial_id, spp, trial_type, qualitative_vigour, geometry 
                                                             from offsite_planting 
                                                             join offsite_site using (trial_id) 
                                                             where trial_type in ('",paste(input$trialType,collapse = "','"),
                                                             "') and spp in ('", paste(substr(input$sppPick2,1,2),collapse = "','"),
                                                             "')"))
                      }else{
                        dat2 <- st_read(sppDb,query = paste0("select offsite_planting.trial_id, spp, trial_type, qualitative_vigour, geometry 
                                                             from offsite_planting 
                                                             join offsite_site using (trial_id) 
                                                             where trial_type in ('",paste(input$trialType,collapse = "','"),
                                                             "') and plantingyear > '", input$trialStart2[1],"' and plantingyear < '",input$trialStart2[2],
                                                             "' and spp in ('", paste(substr(input$sppPick2,1,2),collapse = "','"),
                                                             "')"))
                      }
                      
                    }
                    dat2$label <- dat2$trial_id
                    if("GOM" %in% input$trialType){
                      dat <- dbGetQuery(gomDb, paste0("select latitude,longitude,trial_info.trial_id label,species spp, planting_info.trial_id trial_id 
                                                        from planting_info
                                                        JOIN trial_info
                                                        ON (planting_info.trial_id = trial_info._id)
                                                        where species in ('",paste(substr(input$sppPick2,1,2),collapse = "','"),"')")) %>% unique()
                      
                      if(nrow(dat) > 0){
                        dat <- st_as_sf(dat, coords = c("longitude","latitude"), crs = 4326)
                        dat$qualitative_vigour = NA
                        dat$trial_type = "GOM"
                      }
                      if(nrow(dat2) == 0) dat2 <- dat
                      else dat2 <- rbind(dat,dat2)
                    }
                    if(nrow(dat2) == 0){
                      dat2 <- NULL
                      leafletProxy("offsiteMap") %>%
                        clearMarkers()
                    }else{
                      #browser()
                      plotLocs <- unique(dat2["trial_id"])
                      dat <- as.data.table(st_drop_geometry(dat2))
                      dat[,qualitative_vigour := as.character(qualitative_vigour)]
                      dat[is.na(qualitative_vigour), qualitative_vigour := "UN"]
                      dat[assID, ID := i.ID, on = c(qualitative_vigour = "assessment")]
                      dat <- dat[,.(ID = max(ID)), by = .(trial_id,spp,trial_type,label)]
                      dat[assCols, Col := i.Col, on = "ID"]
                      dat[,label := paste0("Name: ",label)]
                      dat[,Col := as.character(Col)]
                      dat[symbolGuide, Symbol := i.symbol, on = c("trial_type")]
                      #updateSelectInput(session,"trialSelect",choices = unique(dat$trial_id))
                      plotLocs <- st_as_sf(merge(plotLocs, dat, by = "trial_id"))
                      temp <- as.data.table(st_coordinates(plotLocs))
                      setnames(temp, c("long","lat"))
                      plotLocs <- cbind(plotLocs, temp)
                      vals <- unique(plotLocs$trial_type)
                      symbols <- symbolGuide[trial_type %in% vals,symbol]
                      #print(vals)
                      # observeEvent(input$offsiteMap_zoom,{
                      #   print(input$offsiteMap_zoom)
                        leafletProxy("offsiteMap") %>%
                          addSymbols(data = plotLocs,
                                     lat = ~ lat,
                                     lng = ~ long,
                                     values = ~ trial_type,
                                     shape = symbols,
                                     fillOpacity = 0.9,
                                     color = ~ Col,
                                     width = 9,
                                     layerId = ~ trial_id,
                                     label = ~ label)
                      
                    }
                  }
                    
                })

###end offsite tab##