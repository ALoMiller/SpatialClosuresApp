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
             wellPanel(
               dateRangeInput("dates", label = h4("Date range"),
                              start= '2021-01-01'),
               # selectInput("geartype", "Select gear type:",
               #             choices =  c("GILLNET','TRAP/POT"), selected = NULL, multiple =TRUE),
               selectInput("region", "Select region(s):",
                           unique(sc.g2$region),
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
    date.range <- input$dates
    jul.range <- as.numeric(format(date.range, "%j")) #converts shiny inpute date range into julian days
    print(jul.range[1])
    print(jul.range[2])
    sc.g2sub <- sc.g2[sc.g2$region %in% input$region,]
    # sc.g2sub <- sc.g2 %>%
    #   filter('region' %in% input$region)# &
                        #'day_of_the_year_range_1_1' >= jul.range[1] & 
                        #'day_of_the_year_range_1_2' <= jul.range[2])
    print(head(sc.g2sub))
  
    #if statement here to determine which polygons to add to the leaflet map
    for(i in unique(sc.g2sub$shapefile)){
      leafletProxy("base_map") %>%
      # clearShapes() %>% clearMarkers %>% clearPopups() %>%
      # 
      
        addPolygons(data = shapes[[i]],
                           stroke = TRUE, color = '#5a5a5a', opacity = 1.0,
                           weight = 0.5, fillColor = "#dcdcdc", fillOpacity = 0.3, 
                           popup =  paste("Closure period: ",sc.g2sub$closure_period[sc.g2$shapefile==i], "<br>",
                                          "Applies to: ",sc.g2sub$applies_to[sc.g2$shapefile==i], "<br>",
                                          "Exemption: ",sc.g2sub$exempted_gear_fishery[sc.g2$shapefile==i]))# %>%
          
          
        }
      
      
       
  })
}

shinyApp(ui, server)
  