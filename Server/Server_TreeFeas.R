### Kiri Daust
### Feasibility tab

observeEvent(input$showinstr,{
  shinyalert(title = "Instructions",html = T,text = instr_feas)
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
  if(!input$updatedfeas){
    globalFeas$dat <- "newfeas"
  }else{
    globalFeas$dat <- "feasible"
  }
  
}, priority = 20)

output$downloadFeas <- downloadHandler(
  filename = "FeasibilityUpdates.csv",
  content = function(file){
    dat <- dbGetQuery(sppDb,"SELECT bgc,ss_nospace,sppsplit,feasible,newfeas,mod FROM feasorig")
    dat <- as.data.table(dat)
    setnames(dat, old = "sppsplit",new = "spp")
    fwrite(dat, file)
  }
)

##download buttons
output$downloadAudit <- downloadHandler(
  filename = "FeasibilityAudit.csv",
  content = function(file){
    dat <- dbGetQuery(sppDb,"SELECT * FROM feas_audit")
    dat <- as.data.table(dat)
    fwrite(dat, file)
  }
)

output$downloadPest <- downloadHandler(
  filename = "PestUpdates.csv",
  content = function(file){
    dat <- dbGetQuery(sppDb,"SELECT * FROM forhealth")
    dat <- as.data.table(dat)
    fwrite(dat, file)
  }
)

observeEvent(input$sppPick,{
  output$downloadFeasMap <- downloadHandler(
    filename = paste0("TheBECZone_Feasibility_",substr(input$sppPick,1,2),".gpkg"),
    content = function(file){
      if(is.null(input$edaplot_selected)){
        Q1 <- paste0("SELECT bgc_simple.bgc, temp.feasible, temp.spp, bgc_simple.geom
                    FROM bgc_simple
                    JOIN (SELECT bgc, spp, MIN(newfeas) feasible
                          FROM feasorig
                          WHERE spp like '",substr(input$sppPick,1,2),"'
                          GROUP BY bgc, spp) temp
                    ON (bgc_simple.bgc = temp.bgc)
                    WHERE temp.feasible IN (1,2,3)")
      }else{
        id <- as.numeric(input$edaplot_selected)
        idSub <- idDat[ID == id,.(ID,edatopic)]
        tempval <- paste(idSub$edatopic,collapse = "','")
        Q1 <- paste0("
        WITH tempeda(eda) AS
        (values ('",tempval,"')),
        siteseries AS (
          SELECT ss_nospace, edatopic
          FROM eda
          JOIN tempeda
          ON eda.edatopic = tempeda.eda
        )
        
        SELECT bgc_simple.bgc, temp.feasible, temp.spp, bgc_simple.geom
                    FROM bgc_simple
                    JOIN (SELECT bgc, spp, AVG(newfeas) feasible
                          FROM feasorig
                          JOIN siteseries
                          ON feasorig.ss_nospace = siteseries.ss_nospace
                          WHERE spp like '",substr(input$sppPick,1,2),"'
                          GROUP BY bgc, spp) temp
                    ON (bgc_simple.bgc = temp.bgc)
                    WHERE temp.feasible < 3.5")
      }
      
      dat <- st_read(sppDb,query = Q1)
      st_write(dat,file)
    }
  )
})

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

##add plot locations
observeEvent({c(input$showtrees,
                input$sppPick,
                input$trials,
                input$trialStart)},{
                  sppName <- substr(input$sppPick,1,2)
                  if(!is.null(input$showtrees)){
                    QRY <- paste0("select spp,plotnum,geometry from plotdata where spp = '",sppName,"' and region in ('",
                                  paste(input$showtrees,collapse = "','"),"')")
                    dat <- st_read(sppDb,query = QRY)
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
                    #browser()
                    dat2 <- st_read(sppDb, query = paste0("select offsite_planting.trial_id, spp, trial_type, qualitative_vigour, geometry 
                                                             from offsite_planting 
                                                             join offsite_site using (trial_id) 
                                                             where trial_type in ('",paste(input$trials,collapse = "','"),
                                                          "') and spp in ('", sppName,
                                                          "') and plantingyear > '", input$trialStart[1],"' and plantingyear < '",input$trialStart[2],"'"))

                    if(nrow(dat2) == 0){
                      dat2 <- NULL
                      leafletProxy("map") %>%
                        removeGlPoints("tree_trial")
                    }else{
                      plotLocs <- unique(dat2["trial_id"])
                      dat <- as.data.table(st_drop_geometry(dat2))
                      dat[,qualitative_vigour := as.character(qualitative_vigour)]
                      dat[is.na(qualitative_vigour), qualitative_vigour := "UN"]
                      dat[assID, ID := i.ID, on = c(qualitative_vigour = "assessment")]
                      dat <- dat[,.(ID = max(ID)), by = .(trial_id,spp,trial_type)]
                      dat[assCols, Col := i.Col, on = "ID"]
                
                      dat[,Col := as.character(Col)]
                      plotLocs <- merge(plotLocs, dat, by = "trial_id")
                      
                      leafletProxy("map") %>%
                        addGlPoints(data = plotLocs,layerId = "tree_trial",
                                    fillColor = plotLocs$Col,fillOpacity = 1)
                    }
                  }else{
                    leafletProxy("map") %>%
                      removeGlPoints("tree_trial")
                  }
                })

##Prepare BGC colour table for non-edatopic
prepDatSimple <- reactive({
  QRY <- paste0("select bgc,feasorig.ss_nospace,sppsplit,spp,outrange,",globalFeas$dat,
                " FROM feasorig 
                JOIN special_ss
                USING (ss_nospace)
                WHERE special_code IS NULL 
                AND spp = '",substr(input$sppPick,1,2),
                "' AND ",globalFeas$dat," in (1,2,3,4,5)")
  d1 <- dbGetQuery(sppDb, QRY)
  if(nrow(d1) == 0){
    shinyalert(title = "Oopsie doopsie!",text = "There are no data for that species",
               type = "error",showConfirmButton = T)
    QRY <- paste0("select bgc,ss_nospace,sppsplit,spp,outrange,",globalFeas$dat,
                  " from feasorig where spp = 'Sx' and ",globalFeas$dat," in (1,2,3,4,5)")
    d1 <- dbGetQuery(sppDb, QRY)
  }
  feas <- as.data.table(d1)
  if(!input$showOHR){
    feas <- feas[outrange != TRUE,]
  }
  setnames(feas, old = globalFeas$dat, new = "feasible")
  feasMax <- feas[,.(SuitMax = min(feasible)), by = .(bgc,sppsplit)]
  if(input$showFreq){
    feasMax <- prepFreq()
    sppOpts <- unique(feasMax$sppsplit)
    feasMax[,sppsplit2 := as.numeric(as.factor(sppsplit))]
    feasMax[taxaFreqCols, Col := i.Col, on = c(sppsplit2 = "sppsplit","Freq")]
    feasMax[Freq == "Added",Col := "#fbff00ff"]
    feasMax[Freq == "Removed",Col := "#8300ffff"]
    temp <- sort(unique(feasMax$sppsplit))
    #tempTab <- data.table(sppsplit = temp, Col = taxaCols[1:length(temp)])
    if(input$showadd){
      PALeg <- list(
        labels = c(paste(rep(temp,each = 3),
                          rep(c("Very Frequent","Frequent","Infrequent"),
                              length(temp)),sep = ": "),"Added","Removed"),
        colours = c(taxaFreqCols$Col[1:(length(temp)*3)],"#fbff00ff","#8300ffff"),
        title = "Presence/Absence"
      )
    }else{
      PALeg <- list(
        labels = c(paste(rep(temp,each = 3),
                         rep(c("Very Frequent","Frequent","Infrequent"),
                             length(temp)),sep = ": ")),
        colours = c(taxaFreqCols$Col[1:(length(temp)*3)]),
        title = "Presence/Absence"
      )
    }
  }else{
    if(length(unique(feasMax$sppsplit)) > 1){
      temp <- unique(feasMax$sppsplit)
      tempTab <- data.table(sppsplit = temp, Col = taxaCols[1:length(temp)])
      feasMax[tempTab,Col := i.Col, on = "sppsplit"]
      temp <- unique(feasMax[,.(sppsplit,Col)])
      if(input$showadd){
        feasMax[SuitMax == 4,Col := "#fbff00ff"]
        feasMax[SuitMax == 5,Col := "#8300ffff"]
      }else{
        feasMax <- feasMax[!SuitMax %in% c(4,5),]
      }
      
    }else{
      temp <- unique(feasMax$sppsplit)
      tempTab <- data.table(sppsplit = temp, Col = taxaCols[1])
      feasMax[,Col := tempTab$Col[1]]
      if(input$showadd){
        feasMax[SuitMax == 4,Col := "#fbff00ff"]
        feasMax[SuitMax == 5,Col := "#8300ffff"]
      }else{
        feasMax <- feasMax[!SuitMax %in% c(4,5),]
      }
    }
    if(input$showadd){
      PALeg <- list(
        labels = c(tempTab$sppsplit,"Added","Removed"),
        colours = c(tempTab$Col,"#fbff00ff","#8300ffff"),
        title = "Presence/Absence"
      )
    }else{
      PALeg <- list(
        labels = c(tempTab$sppsplit),
        colours = c(tempTab$Col),
        title = "Presence/Absence"
      )
    }
  }
  
  globalLeg$Legend <- PALeg
  feasMax[,Lab := bgc]
  feasMax[,.(bgc,Col,Lab)]
})

##prep frequency colours
prepFreq <- reactive({
  QRY <- paste0("select bgc,feasorig.ss_nospace,sppsplit,spp,outrange,",globalFeas$dat,
                " FROM feasorig 
                JOIN special_ss
                USING (ss_nospace)
                WHERE special_code IS NULL 
                AND spp = '",substr(input$sppPick,1,2),
                "' AND ",globalFeas$dat," in (1,2,3,4,5)")
  feas <- as.data.table(dbGetQuery(sppDb, QRY))
  if(!input$showOHR){
    feas <- feas[outrange != TRUE,]
  }
  setnames(feas, old = globalFeas$dat, new = "feasible")
  edaTemp <- eda[ss_nospace %in% feas$ss_nospace,.(ss_nospace,smr)]
  edaTemp <- edaTemp[,.(smr = mean(smr)), by = .(ss_nospace)]
  edaTemp[,smr := as.integer(round(smr))]
  feasAdd <- feas[!feasible %in% c(1,2,3),]
  feas <- feas[feasible %in% c(1,2,3),]
  feas[edaTemp,SMR := i.smr, on = "ss_nospace"]
  feas[freq_rules,Freq := i.FreqCode, on = c(feasible = "Feasible","SMR")]
  feas <- feas[,.(Freq = max(Freq,na.rm = T)), by = .(bgc,sppsplit)]
  feas[,Freq := as.character(Freq)]
  allFreq <- feas
  
  if(nrow(feasAdd) > 0 & input$showadd){
    feasAdd <- feasAdd[,.(feasible = feasible[1]), by = .(bgc,sppsplit)]
    feasAdd[,Freq := NA_character_]
    feasAdd[feasible == 4,Freq := "Added"]
    feasAdd[feasible == 5,Freq := "Removed"]
    feasAdd[,feasible := NULL]
    allFreq <- rbind(allFreq, feasAdd)
  }
  allFreq
})

##Prepare BGC colours for edatopic option
prepEdaDat <- reactive({
  QRY <- paste0("select bgc,feasorig.ss_nospace,sppsplit,spp,outrange,",globalFeas$dat,
                " FROM feasorig 
                JOIN special_ss
                USING (ss_nospace)
                WHERE special_code IS NULL 
                AND spp = '",substr(input$sppPick,1,2),
                "' AND ",globalFeas$dat," in (1,2,3)")
  feas <- as.data.table(dbGetQuery(sppDb, QRY))
  if(!input$showOHR){
    feas <- feas[outrange != TRUE,]
  }
  setnames(feas, old = globalFeas$dat, new = "feasible") 
  #browser()
  globalLeg$Legend <- edaLeg
  id <- as.numeric(input$edaplot_selected)
  idSub <- idDat[ID == id,.(ID,edatopic)]
  edaSub <- eda[idSub, on = "edatopic"]
  feasSub <- feas[ss_nospace %chin% edaSub$ss_nospace,]
  if(nrow(feasSub) == 0){
    return(NULL)
  }
  feasSub[,Lab := paste0(ss_nospace,": ", feasible)]
  feasSum <- feasSub[,.(FeasVal = round(mean(feasible)/0.5)*0.5, Lab = paste(Lab, collapse = "<br>")), by = bgc]
  feasSum[edaFreqCols,Col := i.Col, on = "FeasVal"]
  #browser()
  feasSum[,.(bgc,Col,Lab)]
})

##render feasibiliy map
observeEvent({c(
  input$sppPick,
  input$edaplot_selected,
  input$updatedfeas,
  input$showFreq,
  input$showadd,
  input$showOHR)
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
  QRY <- paste0("select bgc,feasorig.ss_nospace,special_code, sppsplit, spp, outrange, ",globalFeas$dat,
                " from feasorig JOIN special_ss
                USING (ss_nospace) 
                where bgc = '",unit,"' and ",globalFeas$dat," in (1,2,3,4)")
  feas <- as.data.table(dbGetQuery(sppDb, QRY))
  ohrdat <- feas[spp == substr(input$sppPick,1,2),.(ss_nospace,outrange)]
  feas[,outrange := NULL]
  #browser()
  if(nrow(feas) == 0){
    shinyalert("Oopsies!","There are no species in that subzone :(")
    return(list(dat = feas, rIdx = NULL, cIdx = NULL, sppCol = NULL))
  }
  setnames(feas, old = globalFeas$dat, new = "feasible")   
  feas[ohrdat, OHR := i.outrange, on = "ss_nospace"]
  if(is.null(input$edaplot_selected)){
    feasSub <- feas[sppsplit != "X",]
    tabOut <- data.table::dcast(feasSub, ss_nospace + special_code + OHR ~ sppsplit,fun.aggregate = mean, value.var = "feasible")
    tabOut[,lapply(.SD,as.integer),.SDcols = -"ss_nospace"]
  }else{
    id <- as.numeric(input$edaplot_selected)
    idSub <- idDat[ID == id,.(ID,edatopic)]
    edaSub <- eda[idSub, on = "edatopic"]
    edaSub <- edaSub[bgc == unit,]
    dat <- feas[ss_nospace %in% edaSub$ss_nospace & feasible %in% c(1,2,3,4),]
    tabOut <- data.table::dcast(dat, ss_nospace + special_code + OHR ~ sppsplit, value.var = "feasible", fun.aggregate = mean)
    tabOut[,lapply(.SD,as.integer),.SDcols = -c("ss_nospace","special_code")]
    if(!input$updatedfeas){
      QRY <- paste0("select ss_nospace,sppsplit,feasible from feasorig where bgc = '",
                    unit,"' and feasible in (1,2,3,4)")
      feasOrig <- as.data.table(dbGetQuery(sppDb, QRY))
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
                      rhandsontable(data = dat,readOnly = FALSE, col_highlight = temp$cIdx,
                                    row_highlight = temp$rIdx, spp_highlight = temp$sppCol) %>%
                        hot_col(3, type = "checkbox", readOnly = FALSE) 
            #           %>%
            #             hot_cols(format = "0", renderer = "
            #     function(instance, td, row, col, prop, value, cellProperties) {
            #     Handsontable.renderers.NumericRenderer.apply(this, arguments);
            #     if (instance.params) {
            #         hcols = instance.params.col_highlight
            #         hcols = hcols instanceof Array ? hcols : [hcols]
            #         hrows = instance.params.row_highlight
            #         hrows = hrows instanceof Array ? hrows : [hrows]
            #         hspp = instance.params.spp_highlight
            #         hspp = hspp instanceof Array ? hspp : [hspp]
            #     }
            #     
            #     var i;
            #     for(i = 0; i < 100; i++){
            #         if (instance.params && (col === hcols[i] && row === hrows[i])) {
            #           td.style.background = 'yellow';
            #         }
            #         if(instance.params && col === hspp[i]){
            #             td.style.background = 'lightgreen';
            #         }
            #     }
            #         
            # }
            #                  ")
                    })
                  }
                })

##ask for initials and call sendToDb
observeEvent(input$submitdat,{
  shinyalert("Enter your initials:", type = "input",imageUrl = "images/puppy1.jpg",imageHeight = "100px", inputId = "initials", callbackR = sendToDb)
})

##compile and send updates to database
sendToDb <- function(nme){
  #browser()
  dat <- as.data.table(hot_to_r(input$hot))
  ss_sp <- unique(dat[!is.na(special_code),.(ss_nospace,special_code)])
  if(nrow(ss_sp) > 0){
    ss_sp[,comb := paste0("('",ss_nospace,"','",special_code,"')")]
    val <- paste(ss_sp$comb,collapse = ",")
    qry <- paste0("
                  UPDATE special_ss
                  SET special_code = v.sc
                  FROM (values ",val,") AS v(ss,sc)
                  WHERE special_ss.ss_nospace = v.ss
                  ")
    dbExecute(sppDb,qry)
  }
  
  dat[,special_code := NULL]
  unit <- globalSelBEC()
  QRY <- paste0("select bgc,ss_nospace,sppsplit,spp,",globalFeas$dat,
                " from feasorig where bgc = '",unit,"' and ",globalFeas$dat," in (1,2,3,4)")
  datOrig <- as.data.table(dbGetQuery(sppDb, QRY))
  setnames(datOrig,old = c(globalFeas$dat), new = c("feasible"))
  
  datohr <- dat[!is.na(OHR),.(ss_nospace,OHR)]
  setnames(datohr,old = "OHR",new = "outrange")
  dat[,OHR := NULL]
  dat <- melt(dat, id.vars = c("ss_nospace"), value.name = "newfeas", variable.name = "sppsplit")
  dat2 <- datOrig[dat, on = c("ss_nospace","sppsplit")]
  dat2[is.na(feasible),feasible := -1]
  dat2 <- dat2[newfeas != feasible,]
  dat2[,mod := nme]
  datAudit <- dat2[,.(ss_nospace,sppsplit,newfeas,mod)]
  datAudit[,date := Sys.Date()]
  dbWriteTable(sppDb,"feas_audit", datAudit, append = T, row.names = F)
  datNew <- dat2[is.na(bgc),]
  datOld <- dat2[!is.na(bgc),]
  #browser()
  if(nrow(datNew) > 0){
    temp <- data.table(bgc = gsub("/.*","",datNew$ss_nospace),ss_nospace = datNew$ss_nospace,
                       sppsplit = datNew$sppsplit,feasible = NA, 
                       spp = substr(datNew$sppsplit,1,2),newfeas = datNew$newfeas,mod = nme)
    dbWriteTable(sppDb,"feasorig",temp, append = T,row.names = F)
  }
  if(nrow(datOld) > 0){
    dbWriteTable(sppDb, "temp_update", datOld, overwrite = T)
    dbExecute(sppDb,"UPDATE feasorig 
                  SET newfeas = temp_update.newfeas,
                  mod = temp_update.mod,
                  outrange = temp_update.outrange
                  FROM temp_update
                  WHERE feasorig.ss_nospace = temp_update.ss_nospace
                  AND feasorig.sppsplit = temp_update.sppsplit")
    dbExecute(sppDb,"UPDATE feasorig
                  SET newfeas = 5
                  WHERE newfeas IS NULL
                  AND feasible IS NOT NULL")
  }
  if(nrow(datohr) > 0){
    dbWriteTable(sppDb, "temp_update", datohr, overwrite = T)
    dbExecute(sppDb,paste0("UPDATE feasorig 
                  SET outrange = temp_update.outrange
                  FROM temp_update
                  WHERE feasorig.ss_nospace = temp_update.ss_nospace
                  AND feasorig.spp = '",substr(input$sppPick,1,2),"'"))
  }
  
  shinyalert("Thank you!","Your updates have been recorded", type = "info",
             imageUrl = "images/puppy1.jpg",imageHeight = "100px", inputId = "dbmessage")
  
}
##table to add species
output$hot_add <- renderRHandsontable({
  if(!is.null(globalSelBEC())){
    unit <- globalSelBEC()
    edaSub <- unique(eda[bgc == unit,.(bgc,ss_nospace)])
    temp <- data.table(ss_nospace = edaSub$ss_nospace, newfeas = NA_integer_)
    rhandsontable(data = temp)
  }
})

##modal to add species
observeEvent(input$addspp,{
  shinyalert(html = T,
             text = tagList(
               h4("Select a species, add feasibility, then click submit"),
               pickerInput("sppPickAdd",
                           label = NULL,
                           choices = sppSplitList), 
               fluidRow(column(8,rHandsontableOutput("hot_add")),
                        column(4,textInput("addsppMod",label = "Enter your initials:"))),
               
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
                       sppsplit = substr(input$sppPickAdd,1,2),
                       feasible = NA,spp = substr(input$sppPickAdd,1,2), newfeas = dat$newfeas, mod = input$addsppMod)
    
    dat2 <- dat2[!is.na(newfeas),]
    dbWriteTable(sppDb, name = "feasorig", value = dat2, append = T,row.names = F)
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