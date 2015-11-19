dashboard = bsCollapse(id="story",multiple=T, open = "EVALUATE: Flows, Map View (Unconstraint) - click on country to filter",                                         
           bsCollapsePanel("EVALUATE: Flows, Map View (Unconstraint) - click on country to filter",style="info",                      
                           
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
                                                         c("Average Price Diff" = "price",
                                                           "New Capacity" = "capacity"),
                                                         selected="New Capacity"
                                             ))
                             )
                             
                           )                             
           ),   
           
           
           bsCollapsePanel("Dynamic Visualization (Unconstraint)",  style="info",               
                           tags$div(style="height: 400px; width: 100%;",
                                    fluidRow(
                                      column(4,
                                             selectInput("d1seriesgv", "",
                                                         as.character(unique(series1$series)) )
                                      ),
                                      column(4,
                                             sliderInput("d1yeargv", "", 2010, 2031, 2015,1,animate=list(loop=TRUE),ticks=FALSE)
                                      ),
                                      column(4,
                                             ""
                                      )
                                    ), htmlOutput("d1t1gv")
                           )
                           
                           
           ),
           
           bsCollapsePanel("Dynamic Timeseries (Unconstraint)",  style="info",               
                           tags$div(style="height: 400px; width: 100%;",
                                    fluidRow(
                                      column(4,
                                             selectInput("d1seriesgvts", "",
                                                         as.character(unique(series1$series)) )
                                      ),
                                      column(4,
                                             ""
                                      )
                                    ), htmlOutput("d1t1gvts")
                           )
                           
                           
           ),
           
           bsCollapsePanel("EVALUATE: New Capacity (Unconstraint)",  style="info",                                                              
                           fluidRow(                                            
                             column(12,     
                                    div(class='wrapper',tags$style(".highcharts{height: 100px, width: 300px}"),
                                        showOutput("d1t1", "highcharts"))
                             )
                           )
                           
                           
           ),
           bsCollapsePanel("1.1) New Capacity (2 Water Availability Scenarios)",  style="info",                                                              
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
           bsCollapsePanel("2.1) Average Price Difference",  style="info",
                           fluidRow(      
                             column(12,     
                                    div(class='wrapper',tags$style(".highcharts{height: 100px, width: 300px}"),
                                        showOutput("demo2", "highcharts"))
                             )
                           )
           ),
           bsCollapsePanel("4.1) Fuel Cost vs Consumption (Contraint until 2020 with default design)",  style="info",                                                              
                           fluidRow(                                            
                             column(12,     
                                    div(class='wrapper',tags$style(".highcharts{height: 100px, width: 300px}"),
                                        showOutput("demo3", "highcharts"))
                             )
                           )
           ),
           bsCollapsePanel("4.2) Cost vs Sensitivity (Contraint until 2020 with default design)",  style="info",                                                              
                           fluidRow(                                            
                             column(12,     
                                    div(class='wrapper',tags$style(".highcharts{height: 100px, width: 300px}"),
                                        showOutput("demo4", "highcharts"))
                             )
                           )
           ),
           bsCollapsePanel("5.1) Average Price vs Water Availability (Contraint until 2020 with default design)",  style="info",                                                              
                           fluidRow(                                            
                             column(12,     
                                    div(class='wrapper',tags$style(".highcharts{height: 100px, width: 300px}"),
                                        showOutput("demo5", "highcharts"))
                             )
                           )
           ),
           bsCollapsePanel("EVALUATE: Timeseries",style="info",                           
                           showOutput("d1t2a", "highcharts"),
                           showOutput("d1t2b", "highcharts"),
                           showOutput("d1t2c", "highcharts"),
                           showOutput("d1t2d", "highcharts")
           )
) 