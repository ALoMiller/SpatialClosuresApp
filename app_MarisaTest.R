
load("TPData.RData")

#load("PreFormattedData.RData")
library(shiny)
library(shinydashboard)
library(leaflet)
library(magrittr)
library(collapsibleTree)
library(viridis)
library(sf)


## Dendrogram Setup: 
#all_closures <- read.csv("closures_categories6c.csv")
# building colors list
colfunc <- colorRampPalette(c("royalblue3", "lightsteelblue3", "palegreen4", "greenyellow", "gold"))
colorslist <- rep(colfunc(5), times = c(3,4,9,48,90))
#add a palette for the shapefiles and show the legend on the base map
pal <-  colorFactor(palette = viridis_pal(option='inferno')(6), domain = c("Sea Turtles", "Marine Mammal", 
                                                     "New Marine Mammal", "Fishery", "Habitat", "State"))



#Make dynamic ui for method of closure selection:
# method.tabs <- tabsetPanel(
#   id = "params",
#   type = "hidden",
#   tabPanel("Closure",
#            selectInput("area", "Select closed area(s):",
#                        sc.g3$area, selected = NULL, multiple = TRUE)
#   ),
#   tabPanel("Date",
#            dateRangeInput("dates", label = h5(strong("Date range:")),
#                           start=paste0(format(Sys.Date(), "%Y"),'-01-01')),
#            selectInput("geartype", "Select gear type:",
#                        choices =  c('GILLNET','TRAP/POT','GILLNET & TRAP/POT'), selected = NULL, multiple =FALSE),
#            selectInput("region", "Select region(s):",
#                        unique(sc.g3$region), selected = NULL, multiple = TRUE)
#   ),
#   tabPanel("Region",
#            selectInput("region2", "Select region(s):",
#                        unique(sc.g3$region), selected = NULL, multiple = TRUE),
#            selectInput("geartype2", "Select gear type:",
#                        choices =  c('GILLNET','TRAP/POT','GILLNET & TRAP/POT'), selected = NULL, multiple =FALSE)
# 
#   ),
#   tabPanel("Regulation",
#            selectInput("regtype", "Select regulation type:",
#                        sc.g3$reg_type, selected = NULL, multiple = TRUE)
#   ),
# )

method.tabs2 <- tabsetPanel(
  id = "params2",
  type = "hidden",
  tabPanel("Gillnet",
           # selectInput("fishery", "Mesh size:",
           #             choices = c("medium mesh", "large mesh"), selected = NULL, multiple = FALSE)#,
           selectInput("regulation", "Color by:",
                       choices = c("Seasonal", "Number of nets"), selected = NULL, multiple = FALSE)
  ),
  tabPanel("TrapPot",
           selectInput("fishery", "Fishery:",
                       choices = c(#"Lobster and Jonah crab trap/pot", 
                                   "Other trap/pot"), 
                       selected = NULL, multiple = FALSE),
          selectInput("regulation2", "Color by:",
                       choices = c("Trawl length", "Weak buoy line"),
                       selected = NULL, multiple = FALSE)
  ))

## UI ------------------------------------------------------------------------------
ui <- fluidPage(
    tags$style("
              body {
    -moz-transform: scale(0.9, 0.9); /* Moz-browsers */
    zoom: 0.9; /* Other non-webkit browsers */
    zoom: 90%; /* Webkit browsers */
}
              "),
    titlePanel(tagList(img(src = 'noaanefsclogo.PNG'),br(),title='Decision Support Tool Trap/Pot and Gillnet Spatial Closures'),
               tags$head(tags$link(rel = "icon", type = "image/png", href = "favicon.png"))
    ),
      tabsetPanel(
        # tabPanel("Spatial Closures Map",
        #          tags$div(tags$style(HTML( ".dropdown-menu{z-index:10000 !important;}"))),
        #          fluidPage(
        #            fluidRow(
        #              column(3,
        #                br(),
        #                wellPanel(
        #                  selectInput("method", "Display closed areas by:",
        #                              choices = c("Closure","Date","Region","Regulation")),
        #                              method.tabs,
        #                  actionButton("runBtn","SHOW CLOSURES", icon("cogs"), style="color: black; background-color: orange; border-color: grey")
        #                )),
        #              column(7,
        #                 br(),
        #                 shinydashboard::box(width = NULL, solidHeader = TRUE, status = 'primary',
        #                                     leafletOutput('base_map',width="100%",height="80vh"))),
        #              column(2,
        #                     br(),
        #                 wellPanel(
        #                   #Add help text here
        #                   h5(strong("Read Me")),
        #                   p(  " This application is a tool to display and categorize "
        #                       , " closures affecting gillnet and trap/pot fisheries in the Northwest Atlantic "
        #                       , " that are implemented in the Decision Support Tool (DST). The "
        #                       , a("DST", href="https://www.fisheries.noaa.gov/feature-story/decision-support-tool-helpful-those-finding-ways-reduce-whale-entanglement-fishing", target="_blank")
        #                       ," is designed to aid in the decision-making process involved in reducing serious injury "
        #                       , " and mortality in the North Atlantic right whale and other large whales at "
        #                       , " risk of entanglement from fixed gear fisheries. To begin, select a method for subsetting closures "
        #                       , " to be displayed in the map. Additional options for each method allow further "
        #                       , " functionality. To display selections on the map click ", strong("SHOW CLOSURES")
        #                       , " . For a more detailed breakdown of the fishery closures specific "
        #                       , " to gear, such as gillnet type and mesh size, click the ", strong("Dendrogram")
        #                       , " tab."))
        #                     )
        #                 ))),
        tabPanel("Trap/pot Gear Configurations Map",
                 tags$div(tags$style(HTML( ".dropdown-menu{z-index:10000 !important;}"))), #what is this for?
                 fluidPage(
                   fluidRow(
                     column(3,
                       br(),
                       wellPanel(
                         selectInput("gear_type", "Select gear type:",
                         choices =  c('Gillnet','TrapPot'), selected = NULL, multiple =FALSE),
                         method.tabs2,
                         actionButton("runBtn","SHOW AREAS", icon("cogs"),
                                      style="color: black; background-color: green; border-color: grey")
                       )
                       ),
                     column(7,
                        br(),
                        shinydashboard::box(width = NULL, solidHeader = TRUE, status = 'success',
                                            leafletOutput('base_map2',width="100%",height="80vh")))
                        )))#,
        # tabPanel("Dendrogram",
        #          br(),
        #          collapsibleTreeOutput("plot", height = "500px"))
              )
  )



## Server---------------------------------------------------------------------------
server <- function(input, output, session) {
  ###### LEAFLET BASE MAP for when app initially loads  
  # output$base_map = renderLeaflet({
  #   # Makes a leaflet map to visualize management areas
  # 
  #   leaflet() %>%
  #     setView(lng = -68.73742, lat = 35, zoom = 5) %>%
  #     addProviderTiles(providers$Esri.OceanBasemap) %>%
  #     addScaleBar(position = 'bottomright', options = scaleBarOptions(maxWidth = 250)) %>%
  #     addLegend(pal = pal, opacity = 0.4, values = c("Sea Turtles", "Marine Mammal",
  #                                                    "New Marine Mammal", "Fishery", "Habitat", "State"))
  # })
  # 
  # observeEvent(input$method, {
  #   updateTabsetPanel(inputId = "params", selected = input$method)
  # })
  #   observeEvent(input$runBtn,{
  #   #clear any previous shapefiles selected
  #   leafletProxy("base_map") %>%
  #     clearShapes() %>% clearMarkers %>% clearPopups()
  # 
  #     sc.g3sub <-
  #       switch(input$method,
  #              Closure = {
  #                sc.g3 %>%
  #                dplyr::filter(area %in% input$area)
  #                },
  #              Date = {
  #                if(input$geartype == 'GILLNET') gearsub <- sc.g3[grep('Gill',sc.g3$gear_type),'shapename']
  #                if(input$geartype == 'TRAP/POT') gearsub <- sc.g3[grep('Trap|trap',sc.g3$gear_type),'shapename']
  #                if(input$geartype == 'GILLNET & TRAP/POT') gearsub <- sc.g3$shapename
  # 
  #                 jul.range <- as.numeric(format(input$dates, "%j")) #converts shiny input date range into julian days
  #                 if(jul.range[1]>jul.range[2]){
  #                   days <- c(seq(1,jul.range[2]),
  #                     seq(jul.range[1],365))
  #                 } else {
  #                   days <- seq(jul.range[1],jul.range[2])
  #                 }
  #                 #subset closures by overlap of days
  #                 ind <- sapply(ClosureDays,function(x) any(days %in% x))
  #                 namesClosures <- names(ClosureDays[which(ind)])
  #                 Date = sc.g3 %>%
  #                   dplyr::filter(region %in% input$region & # by region
  #                                   shapename %in% namesClosures &
  #                                   shapename %in% gearsub)
  #                 },
  #              Region = {
  #                if(input$geartype2 == 'GILLNET') gearsub2 <- sc.g3[grep('Gill',sc.g3$gear_type),'shapename']
  #                if(input$geartype2 == 'TRAP/POT') gearsub2 <- sc.g3[grep('Trap|trap',sc.g3$gear_type),'shapename']
  #                if(input$geartype2 == 'GILLNET & TRAP/POT') gearsub2 <- sc.g3$shapename
  #                Region =  sc.g3 %>%
  #                  dplyr::filter(region %in% input$region2 & # by region
  #                                  shapename %in% gearsub2)
  #                },
  #              Regulation = {
  #                sc.g3 %>%
  #                  dplyr::filter(reg_type %in% input$regtype)
  #              }
  #       )
  # 
  #   #Plot Leaflet Map shapefiles in a loop
  #  for(k in sc.g3sub$shapename){
  #     leafletProxy("base_map") %>%
  # 
  #       addPolygons(data = shapes[[sc.g3$shapefile[sc.g3$shapename==k]]],
  #                          stroke = TRUE, color = ~pal(sc.g3sub$reg_type[sc.g3sub$shapename==k]),
  #                          opacity = 0.5,
  #                          weight = 0.5, #fillColor = ~pal(reg_type),
  #                         fillOpacity = 0.3, #fill=FALSE,
  #                          popup =  paste("Area Name: ",sc.g3sub$shapefile[sc.g3sub$shapename==k], "<br>",
  #                                         "Region: ",sc.g3sub$region[sc.g3sub$shapename==k], "<br>",
  #                                         "Closure period: ",sc.g3sub$closure_period[sc.g3sub$shapename==k], "<br>",
  #                                         "Gear Type: ",sc.g3sub$gear_type[sc.g3sub$shapename==k], "<br>",
  #                                         "RegType: ",sc.g3sub$reg_type[sc.g3sub$shapename==k], "<br>",
  #                                         "Exemption: ",sc.g3sub$exempted_gear_fishery[sc.g3sub$shapename==k]))#%>%
  # 
  #   }
  # 
  #     })
    
# Makes a second leaflet map to visualize management areas
output$base_map2 = renderLeaflet({


    leaflet() %>%
      setView(lng = -68.73742, lat = 35, zoom = 5) %>%
      addProviderTiles(providers$Esri.OceanBasemap) %>%
      addScaleBar(position = 'bottomright', options = scaleBarOptions(maxWidth = 250))
  })

  observeEvent(input$gear_type, {
    updateTabsetPanel(inputId = "params2", selected = input$gear_type)
  })
    observeEvent(input$runBtn,{

     
      
  gc1sub <- switch(input$gear_type,
               Gillnet = {
                   # if(input$fishery == "medium mesh") gillsub <- gc1[grepl("medium",gc1$fishery) & 
                   #                                               grepl("gill",gc1$gear_type),]
                   # if(input$fishery == "large mesh") gillsub <- gc1[grepl("large",gc1$fishery) & 
                   #                                               grepl("gill",gc1$gear_type),]
                   Gillnet = droplevels(gc1[grepl("gill",gc1$gear_type),])
               },
               TrapPot = {
                   #if(input$fishery == "Lobster and Jonah crab trap/pot") trapsub <- gc1[grepl("lobster",gc1$fishery) & 
                  #                                               grepl("trap",gc1$gear_type),]
                   #if(input$fishery == "Other trap/pot") trapsub <-  
                   TrapPot = droplevels(gc1[grepl("other", gc1$fishery) & 
                                                                 grepl("trap",gc1$gear_type),]) #trapsub
               }
               )
  
  col.by <- switch(input$gear_type,
               Gillnet = {
                   if(input$regulation == "Seasonal") gillcol  <- gc1sub$seasonal
                   if(input$regulation == "Number of nets") gillcol <- gc1sub$min_string_length
                   Gillnet = gillcol
               },
               TrapPot = {
                   if(input$regulation2 == "Trawl length") trapcol  <- gc1sub$min_string_length
                   if(input$regulation2 == "Weak buoy line") trapcol <- gc1sub$max_buoy_line_strength
                   TrapPot = trapcol
               }
               )

      #clear any previous shapefiles selected
    leafletProxy("base_map2") %>%
      clearShapes() %>% clearMarkers %>% clearPopups() %>% clearControls #clearControls removes the legend so it switches based in input
  
    #Plot Leaflet Map shapefiles in a loop
   for(l in gc1sub$shapename){
      leafletProxy("base_map2") %>%
        addPolygons(data = shapes2[[gc1$shapename[gc1$shapename==l]]],
                           stroke = TRUE, color = ~colorFactor(palette = rainbow(length(unique(col.by))),
                           domain = unique(col.by))(col.by[gc1sub$shapename==l]),
                           weight = 0.3, 
                           # fillColor = ~colorFactor(palette = rainbow(10),
                           # domain = levels(col.by))(col.by[gc1sub$shapename==l]),
                           fillOpacity = 0.3, #fill=FALSE,
                           popup =  paste("Area Name: ",gc1sub$area[gc1sub$shapename==l], "<br>",
                                          "Seasonal Requirements?: ",gc1sub$seasonal[gc1sub$shapename==l], "<br>",
                                          "Minimum Length: ",gc1sub$min_string_length[gc1sub$shapename==l], "<br>",
                                          "Maximum Length: ",gc1sub$max_string_length[gc1sub$shapename==l], "<br>",
                                          "Minimum Weak Link Strength: ",gc1sub$max_weak_link_strength[gc1sub$shapename==l], "<br>",
                                          "Maximum Line Strength: ",gc1sub$max_buoy_line_strength[gc1sub$shapename==l], "<br>",
                                          "Twine Size (gillnet): ",gc1sub$twine_size[gc1sub$shapename==l], "<br>",
                                          "Tie-downs (gillnet): ",gc1sub$tie_downs[gc1sub$shapename==l], "<br>"))

   }                            
    leafletProxy("base_map2") %>% addLegend(pal = colorFactor(palette = rainbow(length(unique(col.by))),
                           domain = unique(col.by)), opacity = 0.4, values = unique(col.by))
      })

  # building dendrogram
# output$plot <- renderCollapsibleTree({
#   collapsibleTree(
#   all_closures,
#   hierarchy = c("GearType", "Method", "Mesh", "Closures", "Groundfish"),
#   root = "All Closures",
#   attribute = "leafCount", tooltip=F,
#   linkLength = 170,
#   width = 1400, height = 700, fontSize = 12,
#   #fill = colorslist,
#   fillByLevel = TRUE
# )
# })

}




shinyApp(ui, server)
