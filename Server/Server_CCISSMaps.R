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
  qry <- paste0("
WITH sspred AS (
SELECT * FROM map2km_sspred 
INNER JOIN edatopic USING(ss_nospace)
WHERE edatopic = '",input$cciss_eda,"'
AND period = '",input$cciss_period,"'
),
feas AS (
	SELECT ss_nospace, newfeas FROM feasibility WHERE spp = '",substr(input$cciss_sppPick,1,2),"'
) 
SELECT siteref, sspred.ss_nospace, feas.newfeas, SUM(ss_prob) 
FROM sspred INNER JOIN feas ON (sspred.ss_pred = feas.ss_nospace)
GROUP BY (siteref, sspred.ss_nospace, newfeas);")

dat <- as.data.table(dbGetQuery(con, qry))
feas <- as.data.table(dbGetQuery(con,paste0("SELECT ss_nospace, newfeas from feasibility where spp = '",substr(input$cciss_sppPick,1,2),"'")))
setnames(feas,c("SS_NoSpace","Feasible"))
res <- cciss_feas(dat,feas)
return(res)
}) %>%
  bindCache(input$cciss_sppPick,input$cciss_eda, input$cciss_period)

observeEvent(input$render_cciss,{
  # base raster
  #browser()
  X <- rast("./inputs/BC_Raster.tif")
  newFeas <- get_sspreds()
  print("map2")
  
  newFeas[NewSuit > 4, NewSuit := 4]
  newFeas[,FeasChange := Curr - NewSuit]
  newFeas[,siteref := as.integer(siteref)]
  values(X) <- NA
  if(input$cciss_stat == "Current"){
    X[newFeas$siteref] <- newFeas$Curr
    ColScheme <- c("darkgreen", "dodgerblue1", "gold2", "white")
    X2 <- project(X, "epsg:3857")
    X2 <- round(X2)
    ctab <- data.frame(value = c(1,2,3,4), col = ColScheme)
    coltab(X2) <- ctab
  }else if(input$cciss_stat == "NewFeas"){
    X[newFeas$siteref] <- newFeas$NewSuit
    X2 <- project(X, "epsg:3857")
    ColScheme <- c("darkgreen", "dodgerblue1", "gold2", "white")
    X2 <- project(X, "epsg:3857")
    X2 <- round(X2)
    ctab <- data.frame(value = c(1,2,3,4), col = ColScheme)
    coltab(X2) <- ctab
  }else{
    X[newFeas$siteref] <- newFeas$FeasChange
    X2 <- project(X, "epsg:3857")
    X2 <- round(X2/0.5)*5
    ColScheme <- c("black", brewer.pal(11,"RdBu")[c(1,2,3,4,5)], "grey90", brewer.pal(11,"RdBu")[c(6,7,8,9,10,11)])
    ctab <- data.frame(value = seq(-3,3,0.5)*10, col = ColScheme)
    coltab(X2) <- ctab
  }
  

  leafletProxy("cciss_map") %>%
    clearImages() %>%
    leaflet::addRasterImage(X2, project = FALSE)
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







