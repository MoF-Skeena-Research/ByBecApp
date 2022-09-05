##Kiri Daust

##offsite trials####
observeEvent(input$showinstr_offsite,{
  shinyalert(title = "Instructions",html = T,text = instr_offsite)
})



observeEvent(input$tabs,{
  if(input$tabs == "tab2"){
    proj_names <- dbGetQuery(sppDb, "select distinct trial_type from offsite_site")[,1]
    updatePickerInput(session, "trialType", choices = proj_names, selected = proj_names)
    sppData <- dbGetQuery(sppDb, "select distinct spp from offsite_planting")[,1]
    sppList2 <- sppList
    for(i in 1:length(sppList2)){
      sppList2[[i]] <- sppList2[[i]][substr(sppList2[[i]],1,2) %in% substr(sppData,1,2)]
    }
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
        leaflet::addLayersControl(
          baseGroups = c("Hillshade","Positron","Satellite", "OpenStreetMap"),
          overlayGroups = c("BGCs","Districts","Cities"),
          position = "topright")
    })
    
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

observeEvent(input$addoffsite,{
  shinyalert(html = T,
            text = tagList(
              h2("Add New Data"),
              actionButton("addSite","Add New Trial"),
              actionButton("addPlanting","Add New Planting")
            ),
            showCancelButton = T,
            showConfirmButton = F,
            closeOnClickOutside = T
            )
})

observeEvent(input$addSite,{
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

observeEvent(input$submitoffsite_site,{
  site <- as.data.table(hot_to_r(input$blanksite))
  site[,trial_id := input$newtrial_id]
  site <- as.data.table(st_as_sf(site, coords = c("Long","Lat"), 
                   crs = 4326, agr = "constant"))
  site2 <- rbind(sitenames, site, fill = T)
  site2 <- st_as_sf(site2[-1,])
  st_write(site2, sppDb,"offsite_site",append = T, row.names = F)
  plant <- as.data.table(hot_to_r(input$blankplant))
  if(nrow(plant) > 0){
    plant[,trial_id := input$newtrial_id]
    plant[,spp := substr(sppvar,1,2)]
    plant2 <- rbind(plantingnames,plant, fill =  T)
    dbAppendTable(sppDb,"offsite_planting",plant2)
  }
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
    dat <- setDT(dbGetQuery(sppDb,"select sppcomposition_label, project_name, trial_type, elevation, slope, aspect, bgc, ss_nospace, smr, snr, plantingseason from offsite_site limit 0"))
    datTemp <- data.table(Lat = numeric(),Long = numeric())
    dat <- cbind(datTemp, dat)
    rhandsontable(dat) %>%
      hot_table(minSpareRows = 1)
  })
  
  output$blankplant <- renderRHandsontable({
    dat <- setDT(dbGetQuery(sppDb,"select sppvar, seedlots, seed_class, stocktype, num_planted, assessor_name_qual,assessment_date_qual, qualitative_vigour, siteindex from offsite_planting limit 0"))
    rhandsontable(dat) %>%
      hot_table(minSpareRows = 1)
  })

# observeEvent(input$addoffsite,{
#   shinyalert("Select location or enter manually?",)
# })

observeEvent(input$trialSelect,{
  output$offsite_site <- renderRHandsontable({
    if(input$trialSelect != ""){
      dat <- setDT(dbGetQuery(sppDb,paste0("select trial_id,project_name,sppcomposition_label,plantingyear,bgc from offsite_site where trial_id = '",
                                     input$trialSelect,"'")))
      rhandsontable(dat)
    }
  })
})

observeEvent(input$trialSelect,{
  output$offsite_planting <- renderRHandsontable({
    if(input$trialSelect != ""){
      dat <- setDT(dbGetQuery(sppDb,paste0("select trial_id, spp, seedlots, assessor_name_qual, assessment_date_qual, qualitative_vigour
                                           from offsite_planting where trial_id = '", 
                                           input$trialSelect,"'")))
      rhandsontable(dat) %>%
        hot_col("qualitative_vigour", type = "dropdown", 
                source = c("Fail","Poor","Fair","Good","Excellent","UN"),strict = T)
    }
  })
})

##this part will need to be fixed
observeEvent(input$submitAss,{
  dat <- as.data.table(hot_to_r(input$offsite_planting))
  ##dat[,mod := input$assessMod]
  dbWriteTable(sppDb, "temp_ass_update", dat, overwrite = T)
  dbExecute(sppDb,"UPDATE offsite_planting
                  SET assessor_name_qual = temp_ass_update.assessor_name_qual
                  FROM temp_ass_update
                  WHERE offsite_planting.trial_id = temp_ass_update.trial_id
                  AND offsite_planting.spp = temp_ass_update.spp
                  AND offsite_planting.seedlots = temp_ass_update.seedlots")
  shinyalert("Thank you!","Your updates have been recorded", type = "info", inputId = "assMessage")
})

observeEvent(input$offsiteMap_glify_click,{
  val <- input$offsiteMap_glify_click$data
  nme <- gsub("Name: ","",val)
  updateSelectInput(session,"trialSelect",selected = nme)
})


# output$addTrial <- renderRHandsontable({
#   input$submitTrial
#   rhandsontable(trialInit) %>%
#     hot_col("assessment", type = "dropdown", 
#             source = c("Fail","Poor","Fair","Good","Excellent","UN"),strict = T)
# })

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
  st_write(dat, sppDb, "offsite", append = TRUE)
  shinyalert("Thank you!","Your trial has been recorded", type = "info", inputId = "trialMessage")
  updateTextInput(session,"addTr_id",value = "")
  updateDateInput(session,"addTr_planted",value = as.Date("2000-01-01"))
  updateTextInput(session,"addTr_id",value = "")
},priority = 5)

observeEvent({c(input$trialType,
                input$trialStart2,
                input$sppPick2,
                input$multiSppTrial)},{
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
                    
                    if(nrow(dat2) == 0){
                      dat2 <- NULL
                      leafletProxy("offsiteMap") %>%
                        removeGlPoints("tree_trial")
                    }else{
                      #browser()
                      plotLocs <- unique(dat2["trial_id"])
                      dat <- as.data.table(st_drop_geometry(dat2))
                      # if(input$sppPick2 == "All"){
                      #   dat <- dat[,.(Col = if(all(assessment == "UN")) "#6B6B6B" else "#AD00BD"),
                      #              by = .(plotid)]
                      # }else{
                      dat[,qualitative_vigour := as.character(qualitative_vigour)]
                      dat[is.na(qualitative_vigour), qualitative_vigour := "UN"]
                      dat[assID, ID := i.ID, on = c(qualitative_vigour = "assessment")]
                      dat <- dat[,.(ID = max(ID)), by = .(trial_id,spp)]
                      dat[assCols, Col := i.Col, on = "ID"]
                      
                      dat[,label := paste0("Name: ",trial_id)]
                      dat <- dat[,.(trial_id,label,Col)]
                      dat[,Col := as.character(Col)]
                      updateSelectInput(session,"trialSelect",choices = unique(dat$trial_id))
                      plotLocs <- merge(plotLocs, dat, by = "trial_id")
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