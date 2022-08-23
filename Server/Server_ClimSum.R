observeEvent(input$showinstr_climmap,{
  shinyalert(title = "Instructions",html = T,text = instr_climmap)
})

getInputDat <- function(){
  ####Set up choices
  climsumInputs$BGC.choose <- dbGetQuery(climDb, "SELECT bgc from bgcall")$bgc
  climsumInputs$BGC.chooseBC <- dbGetQuery(climDb, "SELECT bgc from bgcall where region = 'BC'")$bgc
  period.choose <- dbGetQuery(climDb, "SELECT period from periodhist")$period
  climsumInputs$period.choose <- period.choose
  climsumInputs$fp.choose <- dbGetQuery(climDb, "SELECT period from periodfut")$period
  period.ts <- c("1901 - 1930","1931 - 1960","1961 - 1990","1991 - 2020","2021-2040",
                 "2041-2060","2061-2080","2081-2100")
  climsumInputs$period.ts <- period.ts
  climsumInputs$period.other <- period.choose[!period.choose %in% period.ts]
  climsumInputs$stat.choose <- dbGetQuery(climDb, "SELECT stat from statopts")$stat
  var.choose <- dbGetQuery(climDb, "SELECT climvar from climvaropts")[,1]
  climsumInputs$var.choose <- var.choose
  monthly <- var.choose[grep("01|02|03|04|05|06|07|08|09|10|11|12", var.choose)]
  seasonal <- var.choose[grep("_sp|_sm|_at|_wt", var.choose)]
  annual <- var.choose[!var.choose %in% c(monthly,seasonal)]
  climsumInputs$climvar.choose <- list(Annual = annual, Seasonal = seasonal, Monthly = monthly)
  climsumInputs$futScn <- c("ssp126","ssp245","ssp370","ssp585")
}

observeEvent(input$tabs,{
  if(input$tabs == "tab4"){
    ## map interface
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
          overlayGroups = c("Climate","Cities"),
          position = "topright")
    })
    
  }
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

observeEvent({c(input$map_climvar,input$map_scn,input$map_stat)}, {
  if(input$map_stat != "Mean"){
    climsumExtreme$Min = NA
    climsumExtreme$Max = NA
  }else{
    mx <- dbGetQuery(climDb,paste0("SELECT MAX(value) FROM szsum_fut WHERE climvar = '",
                                    input$map_climvar, "' and stat = 'mean' and scenario = '",input$map_scn,"'"))$max
    mn <- dbGetQuery(climDb,paste0("SELECT MIN(value) FROM szsum_curr WHERE climvar = '",
                                    input$map_climvar, "' and stat = 'mean'"))$min
    climsumExtreme$Min = mn
    climsumExtreme$Max = mx
  }
},priority = 20)

observeEvent({c(input$map_period,input$map_climvar,input$map_scn,input$map_stat)},{
  period <- input$map_period
  climvar <- input$map_climvar
  scn <- input$map_scn
  if(input$map_stat == "Mean"){
    futstat <- "mean"
    currstat <- "mean"
  }else{
    futstat <- "SD.Geo"
    currstat <- "st.dev.Geo"
  }

  if(period != "" & climvar != "" & scn != ""){
    if(period %in% c("2021-2040","2041-2060","2061-2080","2081-2100")) {
      q1 <- paste0("SELECT bgc, value FROM szsum_fut WHERE period = '",period,"' AND climvar = '",
                   climvar,"' AND scenario = '",scn,"' AND stat = '",futstat,"'")
    }else{
      q1 <- paste0("SELECT bgc, value FROM szsum_curr WHERE period = '",period,"' AND climvar = '",
                   climvar,"' AND stat = '",currstat,"'")
    }
    
    dat <- as.data.table(dbGetQuery(climDb,q1))
    dat <- unique(dat, by = "bgc")
    dat[,lab := paste0(bgc,"<br>",round(value,2))]
    dat[,col := colour_values(-1*c(climsumExtreme$Min,climsumExtreme$Max,value), palette = "rdylgn")[-(1:2)]]
    climsumCols$Data <- dat[,.(bgc,col,lab)]
  }
},priority = 10)

observe({
  dat <- climsumCols$Data
  if(nrow(dat) > 0){
    session$sendCustomMessage("colourClimate",dat)
  }
})

###summary pop-up graphs
getData <- reactive({
  selectVar <- input$map_climvar
  period.curr <- c("1901 - 1930","1931 - 1960","1961 - 1990","1991 - 2020")
  period.fut <- c("2021-2040","2041-2060","2061-2080","2081-2100")

  selectBGC <- input$sz.choose
  tabCurr <- "szsum_curr"
  tabFut <- "szsum_fut"
  #browser()
  q1 <- paste0("SELECT bgc, period,stat, climvar, value FROM ",
               tabCurr," WHERE bgc = '",input$clim_click,"' AND period IN ('",paste(period.curr,collapse = "','"),
               "') AND climvar = '",selectVar,"'")
  q2 <- paste0("SELECT bgc, period,stat, climvar, value FROM ",
               tabFut," WHERE bgc = '",input$clim_click,"' AND period IN ('",paste(period.fut,collapse = "','"),
               "') AND climvar = '",selectVar,"' AND scenario = '",input$map_scn,"'")
  climSubset <- dbGetQuery(climDb, q1)
  futureSub <- dbGetQuery(climDb, q2)
  ##browser()
  climSubset <- as.data.table(rbind(climSubset, futureSub))
  climSubset <- dcast(climSubset, period+stat+climvar~bgc, fun.aggregate = mean)
  setorder(climSubset,climvar)
  setnames(climSubset, old = c("period","stat","climvar"), new = c("TimePeriod","Statistic","ClimateVar"))
  setcolorder(climSubset,c(1,3,2,4:length(climSubset)))

  return(climSubset)
})

output$climBoxplot <- renderPlot({
  data <- getData()
  graph <- data[Statistic %in% c("mean","max","min","10%","90%","st.dev.Geo","SD.Geo"),]
  graph[,Statistic := fifelse(Statistic %chin% c("st.dev.Geo","SD.Geo"), "StDev", Statistic)]
  graph[,ClimateVar := NULL]
  graph <- melt(graph, id.vars = c("TimePeriod","Statistic"))
  graph <- dcast(graph, variable+TimePeriod~Statistic)
  setnames(graph, old = c("variable","TimePeriod"), new = c("BGC","Period"))
  setorder(graph, Period)
  
  graph[,Period := as.numeric(substring(as.character(Period), first = 1, last = 4))]
  ggplot(graph, aes(x = Period, lower = `10%`, upper = `90%`, middle = mean, 
                                  ymin = min, ymax = max,group = 1:8))+
    geom_boxplot(stat = "identity", position = "dodge")+
    theme_bw() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
    labs(x = "First Year of Normal Period", y = input$map_climvar)
})

observeEvent(input$clim_click,{
  shinyalert(html = T,
             text = tagList(
               h3("Time Series of BGC"),
               plotOutput("climBoxplot")
             ))
})

###################################### graph interface ##################################################
##update interface options
observeEvent(input$byZone,{
  toggleElement(id = "sz.choose",condition = input$byZone != "Zone")
})

observeEvent(input$includeWNA,{
  if(input$includeWNA == "Yes"){
    znChoose <- zones
  }else{
    znChoose <- zonesBC
  }
  updatePickerInput(session,"BGCZone.choose",choices = znChoose)
})

observeEvent(input$BGCZone.choose,{
  if(input$byZone != "Zone"){
    getInputDat()
    t1 <- paste(input$BGCZone.choose, collapse = "|")
    tempChoose <- climsumInputs$BGC.chooseBC
    if(input$includeWNA == "Yes"){tempChoose <- climsumInputs$BGC.choose}
    szChoose <- tempChoose[grep(t1,tempChoose)]
    updatePickerInput(session,"sz.choose", choices = szChoose, selected = szChoose[1])
  }
})

observeEvent(input$tabs,{
  if(input$tabs == "tab5"){
    #browser()
    getInputDat()
    if(input$includeWNA == "Yes"){
      znChoose <- zones
    }else{
      znChoose <- zonesBC
    }
    updatePickerInput(session,"BGCZone.choose",choices = znChoose,selected = "SBS")
    updatePickerInput(session,"periodTS",choices = climsumInputs$period.ts, 
                      selected = c("1961 - 1990","1991 - 2020"))
    updatePickerInput(session,"periodOther",choices = climsumInputs$period.other,selected = NULL)
    updatePickerInput(session, "graph_climvar", choices = climsumInputs$climvar.choose, 
                      selected = c("MAT","MAP"))
  }
})

getData_Plots <- reactive({
  selectVars <- input$graph_climvar
  selectPer <- c(input$periodTS, input$periodOther)
  selectPerFut <- selectPer[selectPer %in% climsumInputs$fp.choose]
  if(input$byZone == "Zone"){
    selectBGC <- input$BGCZone.choose
    tabCurr <- "zonesum_curr"
    tabFut <- "zonesum_fut"
  }else{
    selectBGC <- input$sz.choose
    tabCurr <- "szsum_curr"
    tabFut <- "szsum_fut"
  }
  selectBC <- "BC"
  if(input$includeWNA == "Yes"){selectBC <- "WNA"}
  q1 <- paste0("SELECT bgc, period,stat, climvar, value FROM ",
               tabCurr," WHERE bgc IN ('",paste(selectBGC,collapse = "','"),
               "') AND period IN ('",paste(selectPer,collapse = "','"),"') AND climvar IN ('",
               paste(selectVars,collapse = "','"),"') AND region = '",selectBC,"'")
  q2 <- paste0("SELECT bgc, period,stat, climvar, value FROM ",
               tabFut," WHERE bgc IN ('",paste(selectBGC,collapse = "','"),
               "') AND period IN ('",paste(selectPerFut,collapse = "','"),"') AND scenario = '",
               input$Scenario,"' AND climvar IN ('",
               paste(selectVars,collapse = "','"),"') AND region = '",selectBC,"'")
  
  if(length(selectVars) > 0){
    climSubset <- dbGetQuery(climDb, q1)
    futureSub <- dbGetQuery(climDb, q2)
    ##browser()
    climSubset <- as.data.table(rbind(climSubset, futureSub))
    climSubset <- dcast(climSubset, period+stat+climvar~bgc, fun.aggregate = mean)
    setorder(climSubset,climvar)
    setnames(climSubset, old = c("period","stat","climvar"), new = c("TimePeriod","Statistic","ClimateVar"))
    setcolorder(climSubset,c(1,3,2,4:length(climSubset)))
    
    return(climSubset)
  }
})

### create data table download
output$summaryTable <- renderTable({
  dat <- getData_Plots()
  dat
},bordered = T)

###Download data
output$downloadTable <- downloadHandler(
  filename = "ClimateSummary.csv",
  content = function(file){
    fwrite(getData_Plots(), file)
  }
)

observeEvent(input$showdata,{
  showModal(modalDialog(
               h3("Climate Data"),
               downloadButton("downloadTable","Download Data"),
               tableOutput("summaryTable")
             ))
})

###create main summary plots
summaryPlots <- reactive({
  plots <- list()
  selectVars <- input$graph_climvar
  selectPer <- c(input$periodTS, input$periodOther)
  for(i in 1:length(selectVars)){
    name <- selectVars[i]
    if (length(selectVars) > 0 & length(selectPer) > 0){
      data <- getData_Plots()
      if(input$grType == "Bar" | input$grType == "Line"){
        graph <- data[ClimateVar == selectVars[i] & 
                        (Statistic == "mean" | Statistic == input$Error | Statistic == input$futError),]
        graph[,Statistic := fifelse(Statistic != "mean", "StDev", "mean")]
        graph[,ClimateVar := NULL]
        graph <- melt(graph,id.vars = c("TimePeriod","Statistic"))
        graph <- dcast(graph, variable+TimePeriod~Statistic)
        setnames(graph,old = c("variable","TimePeriod","StDev","mean"), new = c("BGC","Period","Error","Mean"))
        setorder(graph,Period)
        if(input$grType == "Bar"){
          graph[,Period := as.factor(Period)]
          plots[[i]] <- ggplot(graph, aes(x = BGC, y = Mean, fill = Period)) +
            geom_bar(position = position_dodge(), stat = "identity") +
            geom_errorbar(aes(ymin = Mean - Error, ymax = Mean + Error), width = 0.2, 
                          position = position_dodge(0.9))+ theme_bw() + 
            theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
            ggtitle(selectVars[i]) + labs(x = "BGC")
        }else{
          graph$Period <- as.character(graph$Period)
          graph$Period <- as.numeric(substring(graph$Period, first = 1, last = 4))
          plots[[i]] <- ggplot(graph, aes(x = Period, y = Mean, colour = BGC))+
            geom_line()+
            geom_ribbon(aes(ymin = Mean - Error, ymax = Mean + Error), linetype = 2, alpha = 0.1)+
            theme_bw() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
            ggtitle(selectVars[i]) + labs(x = "First Year of Normal Period", y = "")
        }
      }else{
        graph <- data[ClimateVar == selectVars[i] & Statistic %in% c("mean","max","min","10%","90%",input$Error,input$futError),]
        graph[,Statistic := fifelse(Statistic %chin% c(input$Error, input$futError), "StDev", Statistic)]
        graph[,ClimateVar := NULL]
        graph <- melt(graph, id.vars = c("TimePeriod","Statistic"))
        graph <- dcast(graph, variable+TimePeriod~Statistic)
        setnames(graph, old = c("variable","TimePeriod"), new = c("BGC","Period"))
        setorder(graph, Period)
        
        graph[,Period := as.numeric(substring(as.character(Period), first = 1, last = 4))]
        graph[,BGC := as.factor(BGC)]
        plots[[i]] <- ggplot(graph, aes(x = Period, lower = `10%`, upper = `90%`, middle = mean, 
                                        ymin = min, ymax = max,group = 1:(length(selectPer)*length(unique(graph$BGC))), color = BGC))+
          geom_boxplot(stat = "identity", position = "dodge")+
          theme_bw() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
          ggtitle(selectVars[i]) + labs(x = "First Year of Normal Period")
      }
    } 
  }
  return(plots)
})

pHeight <- reactive({
  selectVars <- input$graph_climvar
  if(length(selectVars) < 1) return(0)
  return(400*(ceiling((length(selectVars))/2)))
})

observe({
  output$sumPlots <- renderPlot({
    ptlist <- summaryPlots()
    to_delete <- !sapply(ptlist,is.null)
    ptlist <- ptlist[to_delete] 
    if (length(ptlist)==0) return(NULL)
    
    grid.arrange(grobs=ptlist,ncol=2)
  }, height = pHeight())
})