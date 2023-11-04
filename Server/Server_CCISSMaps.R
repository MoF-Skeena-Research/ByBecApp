library(terra)
library(leaflet)
library(data.table)
library(pool)


# 
# sppDb <- dbPool(
#   drv = RPostgres::Postgres(),
#   dbname = "spp_feas",
#   host = Sys.getenv("BCGOV_HOST"),
#   port = 5432,
#   user = Sys.getenv("BCGOV_USR"),
#   password = Sys.getenv("BCGOV_PWD")
# )

output$cciss_edaplot <- renderGirafe({
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

get_sspreds <- reactive({
    bgc <- setDT(dbGetQuery(con,paste0("select * from mapdata_2km where futureperiod = '",input$cciss_period,"'"))) ##takes about 15 seconds
    setnames(bgc, c("SiteRef","FuturePeriod","BGC","BGC.pred","BGC.prop"))
    
    edaTemp <- data.table::copy(E1)
    edaTemp <- edaTemp[is.na(SpecialCode),]
    edaTemp[,HasPos := if(any(Edatopic == input$cciss_eda)) T else F, by = .(SS_NoSpace)]
    edaZonal <- edaTemp[(HasPos),]
    edaZonal[,HasPos := NULL]
    ##edatopic overlap
    SSPreds <- edatopicOverlap(bgc,edaZonal,E1_Phase,onlyRegular = TRUE)
    return(SSPreds)
}) %>%
  bindCache(input$cciss_period,input$cciss_eda)

observeEvent(input$render_cciss,{
  # base raster
  #browser()
  X <- rast("BC_Raster.tif")
  SSPreds <- get_sspreds()
  print("map2")
  suit <- as.data.table(dbGetQuery(sppDb, paste0("select bgc, ss_nospace, spp, newfeas from feasorig where spp = '",substr(input$cciss_sppPick,1,2),"'")))
  setnames(suit, c("BGC","SS_NoSpace","Spp","Feasible"))
  
  newFeas <- ccissMap(SSPreds,suit,substr(input$cciss_sppPick,1,2))
  
  newFeas[NewSuit > 4, NewSuit := 4]
  newFeas[,FeasChange := Curr - NewSuit]
  newFeas <- unique(newFeas, by = "SiteRef")
  newFeas[,SiteRef := as.integer(SiteRef)]
  values(X) <- NA
  X[newFeas$SiteRef] <- newFeas$FeasChange
  
  leafletProxy("cciss_map") %>%
    leaflet::addRasterImage(X)
})

output$cciss_map <- renderLeaflet({
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
    leaflet::addLayersControl(
      baseGroups = c("Hillshade","Positron","Satellite", "OpenStreetMap"),
      overlayGroups = c("Cities"),
      position = "topright")
})







