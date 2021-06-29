## A collection of web apps for visualising and updating information by BGC
## June 2021
## site author: Kiri Daust kiri.daust@gov.bc.ca
## content author: Will MacKenzie
# Copyright 2021 Province of British Columbia
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

source("Server/AppSetup.R") ##load a prepare data
source("Server/FeasAppSource.R") ##javascript functions
# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("lumen"),
                fluidRow(style = "background-color: #003366;",
                    column(6,img(src = "images/gov3_bc_logo.png",align = "left")),
                         column(6,h1("The By-BEC Portal",style = "color: white;"))),
                tabsetPanel(
                    tabPanel("Tree Feasibility",
                             useShinyalert(),
                             useShinyjs(),
                             fluidPage(
                                 column(3,
                                        h3("Tree Range and Feasibility by BGC"),
                                        br(),
                                        panel(style = "overflow-y:scroll; max-height: 900px; position:relative; align: centre",
                                              pickerInput("sppPick",
                                                          label = "Select a Species",
                                                          choices = c("None",sppList),
                                                          selected = "Cw - western redcedar"),                                         
                                              h4("Map Display"),
                                              checkboxInput("updatedfeas","Show Updated Range and Feasibility",value = F, width = "250px"),
                                              checkboxInput("showFreq","Show Frequency",value = T),
                                              h4("Edatopic Feasibility:"),
                                              girafeOutput("edaplot",height = "350px"),
                                              
                                              checkboxGroupInput("showtrees","Show species plots",choices = c("BC","AB","US"),inline = T),
                                              checkboxGroupInput("trials","Show location of offsite trials",c("AMAT","RESULTS"), inline = T),
                                              sliderInput("trialStart","Filter offsite trials by planting date:",
                                                          min = minStart, max = maxStart, value = c(minStart,maxStart))
                                        )
                                        
                                 ),
                                 column(9,
                                        useShinyjs(),
                                        leafletjs_feas,
                                        
                                        #tags$style(type = "text/css", "#map {height: calc(100vh - 250) !important;}"),
                                        actionButton("showinstr","Click To Show Instructions"),
                                        br(),
                                        leafglOutput("map",height = "70vh"),
                                        br(),
                                        h3("Tree Feasibility Ratings for selected BGC:"),
                                        p("Edit the feasibility values here. When you click submit, 
                                            the updated values will be sent to a database. If you are looking
                                           at updated values, they will be shown with a pink background on the table."),
                                        fluidRow(
                                            uiOutput("tableBGC"),
                                            rHandsontableOutput("hot"),
                                            br(),
                                            hidden(actionBttn("submitdat", label = "Submit Changes!")),
                                            actionBttn("addspp","Add Species"),
                                            h4("Download feasibility data and updates:"),
                                            downloadButton("downloadFeas",label = "Feasibility"),
                                            downloadButton("downloadAudit",label = "Audit Table")
                                        )
                                 )
                             )
                    ),
                    tabPanel("Off-site Trials",
                             column(3,
                                    h2("Offsite Species Trials"),
                                    checkboxGroupInput("trials2","Show location of offsite trials",offsiteProj),
                                    h3("Filters"),
                                    pickerInput("sppPick2",
                                                label = "Select a Species",
                                                choices = c("All",sppList),
                                                selected = "All"),
                                    checkboxInput("multiSppTrial","Only show multi-species trials"),
                                    sliderInput("trialStart2","Filter offsite trials by planting date:",
                                                min = minStart, max = maxStart, value = c(minStart,maxStart)),
                                    br(),
                                    h3("Add Offsite-Trial"),
                                    splitLayout(
                                        selectInput("addTr_proj",label = "Choose Project Name",choices = offsiteProj,multiple = F),
                                        textInput("addTr_id",label = "Enter trial id"),
                                        dateInput("addTr_planted",label = "Enter date planted")
                                    ),
                                    h4("Enter location or click on map:"),
                                    splitLayout(
                                        textInput("addTr_lat","Latitude"),
                                        textInput("addTr_long","Longitude")
                                    ),
                                    rHandsontableOutput("addTrial"),
                                    textInput("trialMod","Enter your initials:"),
                                    actionButton("submitTrial","Submit Trial")
                             ),
                             column(9,
                                    actionButton("showinstr_offsite","Click To Show Instructions"),
                                    leafglOutput("offsiteMap", height = "70vh"),
                                    h3("Trial Info"),
                                    selectInput("trialSelect",
                                                label = "Select a trial, or click on map",
                                                choices = NULL),
                                    h4("Update assessment in table below:"),
                                    rHandsontableOutput("assIn"),
                                    textInput("assessMod",label = "Enter your initials:"),
                                    actionButton("submitAss","Submit Assessment"),
                                    br(),
                                    h4("Download pest data and updates:"),
                                    downloadButton("downloadPest")
                             )
                    ),
                    tabPanel("Forest Health",
                             column(3,
                                    h2("Hazard Rating by Tree and Pest"),
                                    selectInput("fhSpp",
                                                label = "Select Host Species",
                                                choices = c("None",sppList)),
                                    selectInput("pestSpp",
                                                label = "Select Pest",
                                                choices = c("DRN","DRL","IDW"),
                                                multiple = F),
                                    actionButton("showMatrix","Show Pest-by-Host Table"),
                                    h3("Hazard By BGC"),
                                    rHandsontableOutput("fh_hot_long"),
                                    textInput("fhModLong",label = "Enter your initials:"),
                                    actionButton("submitFHLong","Submit Hazard Updates")
                             ),
                             column(9,
                                    actionButton("showinstr_forhealth","Click To Show Instructions"),
                                    h3("Pest by Host map"),
                                    leafletjs_fh,
                                    #tags$style(type = "text/css", "#fhMap {height: calc(100vh - 250) !important;}"),
                                    leafletOutput("fhMap", height = "70vh"),
                                    br(),
                                    span(textOutput("pestDatLabel", inline = T),style= "font-size:22px"),
                                    p("Note: to add a new pest, right-click on the table, select 'add row', and enter 
                                   pest name, code, and hazard ratings."),
                                    rHandsontableOutput("fh_hot"),
                                    textInput("fhMod",label = "Enter your initials:"),
                                    actionButton("submitFH","Submit Hazard Updates")
                             )
                    ),
                    tabPanel("Find a BGC",
                             fluidRow(
                                 column(2,
                                        selectInput("selectBGC","Select Zone", 
                                                    choices = c("(N)",zones), 
                                                    multiple = F,selected = "(N)"),
                                        pickerInput("selectSubzone","Select subzone(s)",
                                                    choices = "",multiple = T, options =  list(
                                                        `actions-box` = TRUE,
                                                        size = 10)
                                        ),
                                        span(textOutput("selectedBEC", inline = T),style= "font-size:24px")
                                 ),
                                 column(10,
                                        leafletOutput("findBGCMap", height = "80vh")
                                 )
                             )
                             
                    ),
                    tabPanel("About",
                             panel(style = "overflow-y:scroll; max-height: 900px; position:relative; align: centre",
                                   h1("About the By BGC Map Apps"),
                                   p("Welcome to THE BEC ZONE!! This site houses a collection
                                     of apps with the function of geographically displaying and modifying forest
                                     information by BGC (biogeoclimatic ecosystem classification).
                                     The site currently has 3 apps: Tree Feasibility, which displays
                                     feasibility by species with various summary types, Off-site Trials,
                                     which shows trials on a map and is intended as a tool for assessing trials, 
                                     and Forest Health, which displays pest severity by pest and host species. All
                                     three apps are intended to aid expert review, and allow updates where
                                     necessary. The site also contains a utility tool, 'Find a BGC', which 
                                     provides and easy way to select BGCs either by location or name. Each app
                                     contains an Intructions button, which will pop-up detailed information 
                                     about the app. Remember: BEC is God!", style = "font-size:18px"),
                                   hr(),
                                   h3("Authors"),
                                   p("Site Author: Kiri Daust"),
                                   p("Content Author: Will MacKenzie"),
                                   p("Please submit issues or PRs to our", a("Github repo",href = "https://github.com/FLNRO-Smithers-Research/ByBecApp"))
                                   )
                             )
                )
                 
                          
                 )

server <- function(input, output, session) {
    globalFeas <- reactiveValues(dat = "feasible")
    globalLocation <- reactiveValues(loc = c(-124.72,54.56), zoom = 4)
    globalLeg <- reactiveValues(Legend = climaticLeg)
    globalSelBEC <- reactiveVal()
    globalAddTrial <- reactiveValues(data = trialInit)
    
    source("Server/Server_TreeFeas.R",local = T)
    source("Server/Server_Offsite.R",local = T)
    source("Server/Server_ForHealth.R",local = T)
    source("Server/Server_FindBGC.R",local = T)
    
    onStop(function() {
        dbDisconnect(conn = con)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

# ###calculate climatic suitability colours
# prepClimSuit <- reactive({
#     QRY <- paste0("select bgc,ss_nospace,sppsplit,spp,",globalFeas$dat,
#                   " from feasorig where spp = '",substr(input$sppPick,1,2),
#                   "' and ",globalFeas$dat," in (1,2,3,4,5)")
#     feas <- as.data.table(dbGetQuery(con, QRY))
#     setnames(feas, old = globalFeas$dat, new = "feasible")
#     tempFeas <- feas[feasible %in% c(1,2,3),]
#     minDist <- tempFeas[,.SD[feasible == min(feasible, na.rm = T)],by = .(bgc)]
#     abUnits <- minDist[grep("[[:alpha:]] */[[:alpha:]]+$",ss_nospace),]
#     noAb <- minDist[!grepl("[[:alpha:]] */[[:alpha:]]+$",ss_nospace),]
#     abUnits <- eda[abUnits, on = "ss_nospace"] ##merge
#     abUnits <- abUnits[,.(Temp = if(any(grepl("C4",edatopic))) paste0(ss_nospace,"_01") else ss_nospace, feasible = feasible[1]),
#                        by = .(bgc,ss_nospace,sppsplit,spp)]
#     abUnits[,ss_nospace := NULL]
#     setnames(abUnits,old = "Temp",new = "ss_nospace")
#     minDist <- rbind(noAb,abUnits)
#     minDist[,ID := if(any(grepl("01", ss_nospace)) & feasible[1] == 1) T else F, by = .(bgc)]
#     green <- minDist[(ID),]
#     green <- green[,.(Col = zonalOpt), by = .(bgc)]
#     
#     minDist <- minDist[ID == F,]
#     minDist[,ID := if(any(grepl("01", ss_nospace))) T else F, by = .(bgc)]
#     blue <- minDist[(ID),]
#     blue <- blue[,.(feasible = min(feasible)), by = .(bgc)]
#     
#     minDist <- minDist[ID == F,]
#     minEda <- eda[minDist, on = "ss_nospace"]
#     minEda <- minEda[,.(AvgEda = mean(smr)), by = .(bgc,ss_nospace,feasible)]
#     minEda <- minEda[,.(Col = fifelse(all(AvgEda >= 3.5),"WET",
#                                       fifelse(all(AvgEda < 3.5), "DRY", splitOpt)), feasible = min(feasible)), by = .(bgc)]
#     temp <- minEda[Col == "DRY",]
#     temp[,Col := NULL]
#     blue <- rbind(blue,temp)
#     red <- minEda[Col == "WET",]
#     minEda <- minEda[!Col %in% c("WET","DRY"),.(bgc,Col)]
#     blue[dryOpt, Col := i.Col, on = "feasible"]
#     red[wetOpt, Col := i.Col, on = "feasible"]
#     blue[,feasible := NULL]
#     red[,feasible := NULL]
#     climSuit <- rbind(green,blue,red,minEda)
#     climSuit <- climSuit[!is.na(bgc),]
#     
#     tf2 <- feas[feasible %in% c(4,5),.(SuitMax = min(feasible)), by = .(bgc)]
#     if(nrow(tf2) > 0){
#         tf2[SuitMax == 4,Col := "#fbff00ff"]
#         tf2[SuitMax == 5,Col := "#8300ffff"]
#         tf2 <- tf2[,.(bgc,Col)]
#         climSuit <- rbind(climSuit, tf2)
#     }
#     return(climSuit)
# })
