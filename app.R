load("PreFormattedData.RData")
library(shiny)
library(leaflet)
library(magrittr)
ui <- 
  fluidPage(
    titlePanel(tagList(img(src = 'noaanefsclogo.PNG'),br(),title='Spatial Closures'),
               tags$head(tags$link(rel = "icon", type = "image/png", href = "favicon.png")
               )
    ),
    fluidRow(
      column(2, #these columns are based on Bootstrap 12-wide grid and must add up to 12 within a fluid Row
             tags$div(tags$style(HTML( ".dropdown-menu{z-index:10000 !important;}"))),
             wellPanel(
               dateRangeInput("dates", label = h4("Date range"),
                              start=paste0(format(Sys.Date(), "%Y"),'-01-01')),
               selectInput("geartype", "Select gear type:",
                           choices =  c('GILLNET','TRAP/POT','GILLNET & TRAP/POT'), selected = NULL, multiple =FALSE),
               selectInput("region", "Select region(s):",
                           unique(sc.g3$region),
                           selected = NULL, multiple = TRUE),
               actionButton("runBtn","SHOW CLOSURES", icon("cogs"), style="color: black; background-color: orange; border-color: grey")
               )),
      column(10,
             tabsetPanel(
               tabPanel("Spatial Closures Map",
                        br(),
                        shinydashboard::box(width = NULL, solidHeader = TRUE, status = 'primary',
                                            leafletOutput('base_map',width="100%",height="80vh"))),
               tabPanel("Alessandra's Dendrogram",
                        br())
             ))
))
server <- function(input, output, session) {
  ###### LEAFLET BASE MAP for when app initially loads  
  output$base_map = renderLeaflet({
    # Makes a leaflet map to visualize management areas
    
    leaflet() %>%
      setView(lng = -68.73742, lat = 42.31386, zoom = 6) %>%
      addProviderTiles(providers$Esri.OceanBasemap) %>%
      addScaleBar(position = 'bottomright', options = scaleBarOptions(maxWidth = 250))
  })

  observeEvent(input$runBtn,{
    #clear any previous shapefiles selected
    leafletProxy("base_map") %>%
      clearShapes() %>% clearMarkers %>% clearPopups()
      
    date.range <- input$dates
    jul.range <- as.numeric(format(date.range, "%j")) #converts shiny input date range into julian days
    print(jul.range[1])
    print(jul.range[2])
    if(jul.range[1]>jul.range[2]){
      days <- c(seq(1,jul.range[2]),
        seq(jul.range[1],365))
    } else {
      days <- seq(jul.range[1],jul.range[2])
    }
    #subset closures by overlap of days
    ind <- sapply(ClosureDays,function(x) any(days %in% x))
    namesClosures <- names(ClosureDays[which(ind)])
    #subset closures by geartype - this could probably be simplified
    if(input$geartype == 'GILLNET') {
    gearsub <- sc.g3[grep('Gill',sc.g3$gear_type),27]
    } 
    if(input$geartype == 'TRAP/POT'){
    gearsub <- sc.g3[grep('Trap|trap',sc.g3$gear_type),27]
    }
    if(input$geartype == 'GILLNET & TRAP/POT') {
    gearsub <- sc.g3$shapename
    }
      
    print(input$geartype)
    #subset data by various inputs
    sc.g3sub <- sc.g3[sc.g3$region %in% input$region & # by region
                        sc.g3$shapename %in% namesClosures &
                        sc.g3$shapename %in% gearsub,]
    print(length(namesClosures))
    print(head(sc.g3sub))
  
    #if statement here to determine which polygons to add to the leaflet map
    for(k in sc.g3sub$shapename){
      leafletProxy("base_map") %>%
       #clearShapes() %>% clearMarkers %>% clearPopups() %>%
      # 
      
        addPolygons(data = shapes[[sc.g3$shapefile[sc.g3$shapename==k]]],
                           stroke = TRUE, color = '#5a5a5a', opacity = 1.0,
                           weight = 0.5, fillColor = "#dcdcdc", fillOpacity = 0.3, 
                           popup =  paste("Area Name: ",sc.g3sub$shapefile[sc.g3sub$shapename==k], "<br>",
                                          "Region: ",sc.g3sub$region[sc.g3sub$shapename==k], "<br>",
                                          "Closure period: ",sc.g3sub$closure_period[sc.g3sub$shapename==k], "<br>",
                                          "Gear Type: ",sc.g3sub$gear_type[sc.g3sub$shapename==k], "<br>",
                                          "Applies to: ",sc.g3sub$applies_to[sc.g3sub$shapename==k], "<br>",
                                          "Exemption: ",sc.g3sub$exempted_gear_fishery[sc.g3sub$shapename==k]))# %>%
          
          
        }
      
      
       
  })
}

shinyApp(ui, server)
  