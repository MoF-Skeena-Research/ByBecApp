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
                tags$head(HTML("<title>Power of BEC</title>")),
                tags$script(src = "https://unpkg.com/@popperjs/core@2"),
                tags$script(src = "https://unpkg.com/tippy.js@6"),
                fluidRow(style = "background-color: #003366;",
                    column(6,img(src = "images/gov3_bc_logo.png",align = "left")),
                         column(6,h1("The By-BEC Portal",style = "color: white;"))),
                tabsetPanel(id = "tabs",
                    tabPanel(value = "tab1", title = "Tree Feasibility",
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
                                              checkboxInput("updatedfeas","Show Original Feasibility",value = F, width = "250px"),
                                              checkboxInput("showadd","Show Additions/Deletions",value = F, width = "250px"),
                                              checkboxInput("showFreq","Show Frequency",value = T),
                                              checkboxInput("showOHR","Show OHR", value = T),
                                              h4("Edatopic Feasibility:"),
                                              girafeOutput("edaplot",height = "350px"),
                                              
                                              checkboxGroupInput("showtrees","Show species plots",choices = c("BC","AB","US"),inline = T),
                                              selectInput("trials","Select Offsite Trials",choices = proj_names, multiple = T),
                                              sliderInput("trialStart","Filter offsite trials by planting date:",
                                                          min = minStart, max = maxStart, value = c(minStart,maxStart)),
                                              downloadButton("downloadFeasMap",label = "Export Spatial")
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
                    tabPanel(value = "tab2", title = "Off-site Trials",
                             tags$style(
                               type = 'text/css',
                               '.modal-dialog { width: fit-content !important; max-width: 75%;}'
                             ),
                             fluidRow(
                                 column(4,
                                        h3("Select Tree Species"),
                                        pickerInput("sppPick2",
                                                    choices = sppList,
                                                    selected = sppList[[1]][1],
                                                    multiple = T,
                                                    options = pickerOptions(actionsBox = T)),
                                        hr(),
                                        h3("Filters"),
                                        checkboxInput("multiSppTrial","Only show multi-species trials"),
                                        # checkboxGroupInput("trials2","Show trials by type", offsiteProj), 
                                        h4("Show Selected Trial Types"),
                                        pickerInput("trialType",
                                                    #label = "Select Species",
                                                    #choices = offsiteProj,
                                                    choices = c("Research", "Operational","GOM", "Other"),
                                                    selected = c("GOM"),
                                                    multiple = TRUE,
                                                    options = pickerOptions(actionsBox = T)),
                                        h4("Find-a-Trial"),
                                        textInput("lat","Latitude"),
                                        textInput("long","Longitude"),
                                        actionButton("goToLocation","Fly Here!"),
                                        h4("Filter offsite trials by planting year:"),
                                        sliderInput("trialStart2", "",
                                                    min = minStart, max = maxStart, value = c(minStart,maxStart),step = 1, sep = ""),
                                        br(),
                                        hr(),
                                        h4("Select Trial on Map or "), actionButton("addoffsite","Add New Trial"),
                                        # selectInput("trialSelect",
                                        #             label = "Select a trial, or click on map",
                                        #             choices = NULL),
                                        h3("Trial Info"),
                                        downloadButton("download_offsite","Download Selected Trials"),
                                        downloadButton("download_offsite_all","Download Filtered Trials"),
                                        actionButton("completeOffsite","Show Entire Table"),
                                        rHandsontableOutput("offsite_site"),
                                        h3("Planting Info"),
                                        p("Add a qualitative assessment; right-click table to add additional species"),
                                        rHandsontableOutput("offsite_planting"),
                                        br(),
                                        actionButton("submitAss","Click to Submit Edits"),
                                        hr(),
                                        br(),
                                        br(),
  
                                 ),
                                 column(8,
                                        actionButton("showinstr_offsite","Click To Show Instructions"),
                                        leafglOutput("offsiteMap", height = "70vh"),
                                        
                                 )
                             ),


                    ),
                    # h3("Existing Trial Information"),
                    # 
                    # h5("add in here fields for trial type, project name, latitude, longitude, elevation, BGC, Site series  "),
                    # h4("Species assessment in table below:"),
                    # rHandsontableOutput("assIn"),
                    # textInput("assessMod",label = "Enter your initials:"),
                    # actionButton("submitAss","Submit Assessment"),
                    # br(),
                    # h4("Download trial data and updates:"),
                    # downloadButton("downloadPest"),
                    # h3("Add New Offsite-Trial"),
                    # splitLayout(
                    #     selectInput("addTr_proj",label = "Choose Project Name",choices = offsiteProj,multiple = F),
                    #     textInput("addTr_id",label = "Enter trial id"),
                    #     dateInput("addTr_planted",label = "Enter date planted")
                    # ),
                    # #h3("Trial Info"),
                    # #selectInput("trialSelect",
                    # #            label = "Select a trial, or click on map",
                    # #            choices = NULL),
                    # 
                    # h4("Enter location or click on map:"),
                    # splitLayout(
                    #     textInput("addTr_lat","Latitude"),
                    #     textInput("addTr_long","Longitude")
                    # ),
                    # rHandsontableOutput("addTrial"),
                    # textInput("trialMod","Enter your initials:"),
                    # actionButton("submitTrial","Submit Trial")
                    tabPanel(value = "tab3",title = "Forest Health",
                             useShinyjs(),
                             extendShinyjs(text = '
                                  shinyjs.selectInput_tooltips = function(params){
                                  var defaultParams = {
                                    id : null,
                                    tooltips : null
                                  };
                                  params = shinyjs.getParams(params, defaultParams);
                        
                                  var selectInput = $("#"+params.id).closest("div").find(".dropdown-menu").get(1);
                                  var element_selectInput = selectInput.childNodes;
                        
                                  if(element_selectInput.length >0 && element_selectInput[0].title == ""){ // to be trigger only once
                                    for(var i = 0; i < element_selectInput.length; i++){
                                      element_selectInput[i].title = params.tooltips[i];
                                    }
                                  }
                                }; 
                              ',
                                           functions = "selectInput_tooltips"),
                             column(3,
                                    h2("Hazard Rating by Tree and Pest"),
                                    selectInput("fhSpp",
                                                label = "Select Host Species",
                                                choices = c("None",sppList)),
                                    pickerInput("pestSpp",
                                                label = "Select Pest",
                                                choices = c("DRN","DRL","IDW"),
                                                multiple = F),
                                    radioButtons("fh_region","Show Results for: ", choices = c("BC","WNA"),
                                                 selected = "BC", inline = T),
                                    splitLayout(actionButton("downloadFH","Download CSV"),
                                                actionButton("uploadFH","Upload CSV"),
                                                downloadButton("downloadFHMap","Download Spatial"))
                                    ,
                                    actionButton("showMatrix","Show Pest-by-Host Table"),
                                    h3("Hazard By BGC"),
                                    panel(style = "overflow-y:scroll; max-height: 500px; position:relative; align: centre",
                                        rHandsontableOutput("fh_hot_long")
                                    ),
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
                                    #panel(style = "overflow-y:scroll; max-height: 500px; position:relative; align: centre",
                                        rHandsontableOutput("fh_hot"),
                                        textInput("fhMod",label = "Enter your initials:"),
                                        actionButton("submitFH","Submit Hazard Updates")
                                    #)
                             )
                    ),
                    navbarMenu("Climate Summaries",
                               tabPanel(value = "tab4", title = "Map Interface",
                                        column(3,
                                               h2("Climate variable summaries by BGC"),
                                               pickerInput("map_period",
                                                           label = "Select Time Period",
                                                           choices = "",
                                                           multiple = F,
                                                           selected = NULL),
                                               pickerInput("map_climvar",
                                                           "Select Climate Variables",
                                                           choices = "",
                                                           inline = FALSE,
                                                           multiple = F,
                                                           selected = NULL),
                                               awesomeRadio("map_stat",
                                                            "Select Display Statistic",
                                                            choices = c("Mean","Variance"),
                                                            inline = T,
                                                            selected = "Mean"),
                                               awesomeRadio("map_scn",
                                                            "Select Emission Scenario",
                                                            choices = "",
                                                            inline = TRUE,
                                                            selected = NULL)
                                        ),
                                        column(9,
                                               h3("Climate by BGC Map"),
                                               actionButton("showinstr_climmap","Click To Show Instructions"),
                                               leafletjs_clim,
                                               leafletOutput("climMap", height = "70vh")
                                        )
                               ),
                               tabPanel(value = "tab5", title = "Plot Interface",
                                            fluidRow(
                                                column(2,
                                                       titlePanel("Select Input"), 
                                                       awesomeRadio("includeWNA",
                                                                    "Include WNA units?",
                                                                    choices = c("No", "Yes"),
                                                                    selected = "No",
                                                                    inline = TRUE),
                                                       
                                                       awesomeRadio("byZone",
                                                                    "Summarize by:",
                                                                    choices = c("Zone","Subzone/variant"),
                                                                    selected = "Subzone/variant",
                                                                    inline = TRUE),
                                                       pickerInput(inputId = "BGCZone.choose",###Select BGCs
                                                                   label = "Select Zones for Summary",
                                                                   choices = "", 
                                                                   multiple = TRUE),
                                                       hidden(pickerInput("sz.choose",
                                                                          label = "Select Subzones",
                                                                          choices = "",
                                                                          multiple = TRUE,
                                                                          options = list(`actions-box` = TRUE))),
                                                       dropdown(
                                                           pickerInput("periodTS",
                                                                       label = "Sequential Normal Periods",
                                                                       choices = "",
                                                                       multiple = TRUE,
                                                                       options = list(`actions-box` = TRUE)),
                                                           pickerInput("periodOther",
                                                                       label = "Other Normal Periods",
                                                                       choices = "",
                                                                       multiple = TRUE,
                                                                       options = list(`actions-box` = TRUE)),
                                                           circle = FALSE, label = "Period", status = "primary"), 
                                                       
                                                           pickerInput("graph_climvar",
                                                                       "Select Climate Variables:",
                                                                       choices = "",
                                                                       inline = FALSE,
                                                                       multiple = TRUE),

                                                       ####Select choices for graphs
                                                       awesomeRadio("Error",
                                                                    "Select Error Type:",
                                                                    choices = c("st.dev.Geo","st.dev.Ann"),
                                                                    selected = "st.dev.Geo",
                                                                    inline = TRUE),
                                                       awesomeRadio("Scenario",
                                                                    "Select Future Scenario",
                                                                    choices = c("ssp126","ssp245","ssp370","ssp585"),
                                                                    selected = "ssp370",
                                                                    inline = TRUE),
                                                       awesomeRadio("futError",
                                                                    "Select Future Error Type",
                                                                    choices = c("SD.Mod","SD.Geo"),
                                                                    selected = "SD.Geo",
                                                                    inline = TRUE),
                                                       awesomeRadio("grType",
                                                                    "Choose Graph Type",
                                                                    choices = c("Bar","Boxplot","Line"),
                                                                    selected = "Bar",
                                                                    inline = TRUE)
                                                ),
                                                column(10,
                                                       titlePanel("Summary Figures"),
                                                       h4("ClimateBC Summary by BGC"),
                                                       actionButton("showdata","Show Data"),
                                                       #downloadButton("downloadSumPlots",label = "Download Plots"),
                                                       plotOutput("sumPlots")
                                                )
                                       )
                               )
                        ),
                    tabPanel(value = "tab6", title = "Find a BGC",
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
                                     about the app.", style = "font-size:18px"),
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
    globalLatLong <- reactiveValues(lat = NA_real_, long = NA_real_, useMap = F)
    globalTrialID <- reactiveValues(ID = NULL)
    climsumInputs <- reactiveValues()
    climsumCols <- reactiveValues(Data = data.table())
    climsumExtreme <- reactiveValues()
    
    source("Server/Server_TreeFeas.R",local = T)
    source("Server/Server_Offsite.R",local = T)
    source("Server/Server_ForHealth.R",local = T)
    source("Server/Server_ClimSum.R",local = T)
    source("Server/Server_FindBGC.R",local = T)
}

# Run the application 
shinyApp(ui = ui, server = server)