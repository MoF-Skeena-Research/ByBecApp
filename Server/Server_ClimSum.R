getInputDat <- function(){
  ####Set up choices
  climsumInputs$BGC.choose <- dbGetQuery(climcon, "SELECT bgc from bgcall")$bgc
  climsumInputs$BGC.chooseBC <- dbGetQuery(climcon, "SELECT bgc from bgcall where region = 'BC'")$bgc
  period.choose <- dbGetQuery(climcon, "SELECT period from periodhist")$period
  climsumInputs$period.choose <- period.choose
  climsumInputs$fp.choose <- dbGetQuery(climcon, "SELECT period from periodfut")$period
  period.ts <- c("1901 - 1930","1931 - 1960","1961 - 1990","1991 - 2020","2021-2040",
                 "2041-2060","2061-2080","2081-2100")
  climsumInputs$period.ts <- period.ts
  climsumInputs$period.other <- period.choose[!period.choose %in% period.ts]
  climsumInputs$stat.choose <- dbGetQuery(climcon, "SELECT stat from statopts")$stat
  var.choose <- dbGetQuery(climcon, "SELECT climvar from climvaropts")[,1]
  climsumInputs$var.choose <- var.choose
  monthly <- var.choose[grep("01|02|03|04|05|06|07|08|09|10|11|12", var.choose)]
  seasonal <- var.choose[grep("_sp|_sm|_at|_wt", var.choose)]
  annual <- var.choose[!var.choose %in% c(monthly,seasonal)]
  climsumInputs$climvar.choose <- list(Annual = annual, Seasonal = seasonal, Monthly = monthly)
  climsumInputs$futScn <- c("ssp126","ssp245","ssp370","ssp585")
}

output$climMap <- renderLeaflet({
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
    addPlugins() %>%
    leaflet::addLayersControl(
      baseGroups = c("Hillshade","Positron","Satellite", "OpenStreetMap"),
      overlayGroups = c("Climate","Districts","Cities"),
      position = "topright")
})

observeEvent(input$tabs, {
  if(input$tabs == "tab4"){
    #browser()
    getInputDat()
    updatePickerInput(session,"map_period",choices = climsumInputs$period.ts,selected = "1961 - 1990")
    updatePickerInput(session,"map_climvar",choices = climsumInputs$climvar.choose,selected = "MAT")
    updateAwesomeRadio(session,"map_scn", choices = climsumInputs$futScn, selected = "ssp370")
    
    leafletProxy("climMap") %>%
      invokeMethod(data = NULL, method = "addClimSumTiles")
  }
})

observeEvent({c(input$map_period,input$map_climvar,input$map_scn)},{
  period <- input$map_period
  climvar <- input$map_climvar
  scn <- input$map_scn

  if(period != "" & climvar != "" & scn != ""){
    if(period %in% c("2021-2040","2041-2060","2061-2080","2081-2100")) {
      q1 <- paste0("SELECT bgc, value FROM szsum_fut WHERE period = '",period,"' AND climvar = '",
                   climvar,"' AND scenario = '",scn,"' AND stat = 'mean'")
    }else{
      q1 <- paste0("SELECT bgc, value FROM szsum_curr WHERE period = '",period,"' AND climvar = '",
                   climvar,"' AND stat = 'mean'")
    }
    
    dat <- as.data.table(dbGetQuery(climcon,q1))
    dat <- unique(dat, by = "bgc")
    dat[,col := colour_values(value, palette = "magma")]
    climsumCols$Data <- dat[,.(bgc,col)]
  }
})

observe({
  dat <- climsumCols$Data
  if(nrow(dat) > 0){
    session$sendCustomMessage("colourClimate",dat)
  }
})