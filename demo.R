dashboard = fluidPage(fluidRow(
  column(3,tags$h5("STEP 1 - EVALUATE/CREATE POLICIES"),
         bsButton("s1s2","Next (Step2) >>",style="primary")
  ),
  column(2,sliderInput("d1water", "Water Availability % (Assumption)", 
                       min(unique(runMasterdata[runMasterdata$policy_id==14,]$water.availability))*100, 
                       max(unique(runMasterdata[runMasterdata$policy_id==14,]$water.availability))*100, 
                       mean(unique(runMasterdata[runMasterdata$policy_id==14,]$water.availability))*100,
                       10,
                       animate=FALSE,ticks=FALSE,width = "100%")),
  column(2,sliderInput("d1uclf", "Coal UCLF % (Assumption)", 
                       min(unique(runMasterdata[runMasterdata$policy_id==14,]$coal.uclf))*100, 
                       max(unique(runMasterdata[runMasterdata$policy_id==14,]$coal.uclf))*100, 
                       max(unique(runMasterdata[runMasterdata$policy_id==14,]$coal.uclf))*100,
                       10,
                       animate=FALSE,ticks=FALSE)),
  column(2,
         sliderInput("d1uclf2", "Transmission UCLF % (Assumption)", 
                     min(unique(runMasterdata[runMasterdata$policy_id==14,]$transmission.uclf))*100, 
                     max(unique(runMasterdata[runMasterdata$policy_id==14,]$transmission.uclf))*100, 
                     max(unique(runMasterdata[runMasterdata$policy_id==14,]$transmission.uclf))*100,
                     10,
                     animate=FALSE,ticks=FALSE) 
  ),
  column(1,checkboxInput("withoutGrandInga", "Without Grand Inga", FALSE)), 
  column(2,
         sliderInput("d1cons", "Consumption % Adjustment", 
                     min(unique(runMasterdata[runMasterdata$policy_id==14,]$consumption.adjustment))*100, 
                     max(unique(runMasterdata[runMasterdata$policy_id==14,]$consumption.adjustment))*100, 
                     mean(unique(runMasterdata[runMasterdata$policy_id==14,]$consumption.adjustment))*100,
                     10,
                     animate=FALSE,ticks=FALSE)) 
),
fluidRow(
  column(3,
         textInput("s1policyname", "Descriptive Policy Name for Assuptions:"),
         bsAlert("s1alert"),
         bsButton("s1createpolicy","Create Policy",style="primary"),
         uiOutput("createlistui"),
         fluidRow(
           column(8,uiOutput("s1info")),
           column(4,
                  "Selected Policy:", tags$br(),   
                  bsButton("s1loadpolicy","Show >",style="info"),
                  tags$br(),tags$br(),
                  bsButton("s1deletepolicy","Delete",style="warning"),
                  #bsButton("s1downloadpolicy","Download",style="info"),
                  bsModal("modalDelete", "Delete Policy: Are you sure?", "s1deletepolicy", size = "small",
                          bsButton("s1deletepolicyY","Yes - Delete",style="warning"),
                          bsButton("s1deletepolicyN","No - Cancel",style="info"))
           )
         ),
         tags$br(),
         tags$h5("Help: Create policies based on assuptions by changing the slider positions (Generation Expantion Plan runs UNCONSTRAINT), then when happy enter a 'Policy Name' and then click 'Create Policy'."),
         tags$h5("Note: Create at least 2 Policies before proceeding to STEP2, Policy names must be at least 6 Characters long."),
         downloadButton('s1downloadpolicy', 'Download (for Slider Selection)')
         
  ),
  column(9,
         bsCollapse(id="story",multiple=T, open = "EVALUATE: Flows, Map View (Unconstraint) - click on country to filter",                                         
                    bsCollapsePanel("EVALUATE: Flows, Map View (Unconstraint) - click on country to filter",style="danger",                      
                                    
                                    bsModal(id="mapmodal", title="New Capacity",trigger=NULL, size = "large",
                                            showOutput("d1mapchart", "highcharts")
                                    ),
                                    
                                    fluidRow(
                                      column(12,  
                                             conditionalPanel(
                                               condition = "output.d1m1==null ",                                    
                                               div(class = "busy",
                                                   p("Loading Map ..."),
                                                   img(src="ajaxloaderq.gif")
                                               )
                                             ),
                                             leafletOutput("d1m1", width="90%",height=600),                               
                                             tags$div(style=" position: absolute;left: 70px;top: 490px;",
                                                      sliderInput("d1year", " Tx Flows Year", 2010, 2031, 2015,1,animate=list(loop=TRUE),ticks=FALSE)
                                             ),
                                             tags$div(style=" position: absolute;left: 20px;top: 570px;",
                                                      imageOutput("m1legend",width = "100%", height = "30px")
                                             ),
                                             tags$div(style=" position: absolute;left: 70px;top: 420px;",
                                                      #imageOutput("m1legend",width = "100%", height = "30px"),
                                                      selectInput("mapview","Map Icon Graphs:",
                                                                  c("New Capacity" = "capacity",
                                                                    "Average Price Diff" = "price")
                                                      ))
                                      )
                                      
                                    )                             
                    ),   
                    
                  
                    
                    bsCollapsePanel("EVALUATE: New Capacity (Unconstraint)",  style="danger",                                                              
                                    fluidRow(                                            
                                      column(12,     
                                             div(class='wrapper',tags$style(".highcharts{height: 100px, width: 300px}"),
                                                 showOutput("d1t1", "highcharts"))
                                      )
                                    )
                                    
                                    
                    ),
                    bsCollapsePanel("1.1) New Capacity (2 Water Availability Scenarios)",  style="danger",                                                              
                                    fluidRow(                                            
                                      column(12,     
                                             div(class='wrapper',tags$style(".highcharts{height: 100px, width: 300px}"),
                                                 showOutput("demo1a", "highcharts")
                                             )
                                      ),
                                      column(12,     
                                             div(class='wrapper',tags$style(".highcharts{height: 100px, width: 300px}"),
                                                 showOutput("demo1b", "highcharts"))
                                      )
                                    )
                    ),
                    bsCollapsePanel("2.1) Average Price Difference",  style="danger",
                                    fluidRow(      
                                      column(12,     
                                             div(class='wrapper',tags$style(".highcharts{height: 100px, width: 300px}"),
                                                 showOutput("demo2", "highcharts"))
                                      )
                                    )
                    ),
                    bsCollapsePanel("4.1) Fuel Cost vs Consumption (Contraint until 2020 with default design)",  style="danger",                                                              
                                    fluidRow(                                            
                                      column(12,     
                                             div(class='wrapper',tags$style(".highcharts{height: 100px, width: 300px}"),
                                                 showOutput("demo3", "highcharts"))
                                      )
                                    )
                    ),
                    bsCollapsePanel("4.2) Cost vs Sensitivity (Contraint until 2020 with default design)",  style="danger",                                                              
                                    fluidRow(                                            
                                      column(12,     
                                             div(class='wrapper',tags$style(".highcharts{height: 100px, width: 300px}"),
                                                 showOutput("demo4", "highcharts"))
                                      )
                                    )
                    ),
                    bsCollapsePanel("5.1) Average Price vs Water Availability (Contraint until 2020 with default design)",  style="danger",                                                              
                                    fluidRow(                                            
                                      column(12,     
                                             div(class='wrapper',tags$style(".highcharts{height: 100px, width: 300px}"),
                                                 showOutput("demo5", "highcharts"))
                                      )
                                    )
                    ),
                    bsCollapsePanel("EVALUATE: Timeseries",style="danger",                           
                                    showOutput("d1t2a", "highcharts"),
                                    showOutput("d1t2b", "highcharts"),
                                    showOutput("d1t2c", "highcharts"),
                                    showOutput("d1t2d", "highcharts")
                    )
         ) 
  )# col
))
