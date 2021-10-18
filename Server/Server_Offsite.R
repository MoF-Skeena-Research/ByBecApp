##Kiri Daust

##offsite trials####
observeEvent(input$showinstr_offsite,{
  shinyalert(title = "Instructions",html = T,text = instr_offsite)
})

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
      dat <- dbGetQuery(sppDb,paste0("select plotid,spp,numplanted,seedlot,assessment from offsite where plotid = '",
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
  dbWriteTable(sppDb, "temp_ass_update", dat, overwrite = T)
  dbExecute(sppDb,"UPDATE offsite
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
  st_write(dat, sppDb, "offsite", append = TRUE)
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
                        dat2 <- st_read(sppDb,query = paste0("select project_id, plotid, spp, seedlot,assessment, geometry from offsite where project_id in ('",paste(input$trials2,collapse = "','"),
                                                           "') and planted > '", input$trialStart2[1],"' and planted < '",input$trialStart2[2],
                                                           "' and plotid in (select plotid from offsite group by plotid having count(distinct spp) > 1)"))
                      }else{
                        dat2 <- st_read(sppDb,query = paste0("select project_id, plotid, spp, seedlot,assessment, geometry from offsite where project_id in ('",paste(input$trials2,collapse = "','"),
                                                           "') and planted > '", input$trialStart2[1],"' and planted < '",input$trialStart2[2],"' and spp like '", substr(input$sppPick2,1,2),
                                                           "%' and plotid in (select plotid from offsite group by plotid having count(distinct spp) > 1)"))
                      }
                    }else{
                      if(input$sppPick2 == "All"){
                        dat2 <- st_read(sppDb,query = paste0("select project_id, plotid, spp, seedlot,assessment, geometry from offsite where project_id in ('",paste(input$trials2,collapse = "','"),
                                                           "') and planted > '", input$trialStart2[1],"' and planted < '",input$trialStart2[2],
                                                           "'"))
                      }else{
                        dat2 <- st_read(sppDb,query = paste0("select project_id, plotid, spp, seedlot,assessment, geometry from offsite where project_id in ('",paste(input$trials2,collapse = "','"),
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