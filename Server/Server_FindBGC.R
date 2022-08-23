##server function for Find BGC Tab
##Kiri Daust


observeEvent(input$tabs,{
  if(input$tabs == "tab6"){
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
  }
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