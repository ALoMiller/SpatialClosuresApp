load("PreFormattedData.RData")
library(shiny)
library(leaflet)

ui <- 
  fluidPage(
    titlePanel(tagList(img(src = 'noaanefsclogo.PNG'),br(),title='Spatial Closures'),
               tags$head(tags$link(rel = "icon", type = "image/png", href = "favicon.png")
               )
    ),
    fluidRow(
      column(2, #these columns are based on Bootstrap 12-wide grid and must add up to 12 within a fluid Row
             wellPanel(
               dateRangeInput("dates", label = h4("Date range")),
               # selectInput("geartype", "Select gear type:",
               #             choices =  c("GILLNET','TRAP/POT"), selected = NULL, multiple =TRUE),
               # selectInput("region", "Select region(s):",
               #             c("ALL", "CUSTOM"),
               #             selected = NULL, multiple = TRUE),
               actionButton("runBtn","SHOW CLOSURES", icon("cogs"), style="color: black; background-color: orange; border-color: grey")
               )),
      column(10,
          shinydashboard::box(width = NULL, solidHeader = TRUE, status = 'primary', 
                              leafletOutput('base_map',width="100%",height="80vh")
                              )
        )
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
    jul.range <- format(date.range, "%j") #converts shiny inpute date range into julian days
    #print(jul.range)
    
  
    #if statement here to determine which polygons to add to the leaflet map
    leafletProxy("base_map") %>% 
        addPolygons(data = shapes[['Massachusetts_Bay_Management_Area']] ,stroke = TRUE, color = '#5a5a5a', opacity = 1.0, 
                    weight = 0.5, fillColor = "#dcdcdc", fillOpacity = 0.3, 
                    popup =  paste("Closure period: ",sc.g2$closure_period[sc.g2$shapefile=='Massachusetts_Bay_Management_Area'], "<br>",
                                             "Applies to: ",sc.g2$applies_to[sc.g2$shapefile=='Massachusetts_Bay_Management_Area'], "<br>",
                                             "Exemption: ",sc.g2$exempted_gear_fishery[sc.g2$shapefile=='Massachusetts_Bay_Management_Area']))# %>%
        
  })
}

shinyApp(ui, server)
  