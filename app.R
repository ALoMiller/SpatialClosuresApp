load("PreFormattedData.RData")
load("TPData.RData")
library(shiny)
library(shinydashboard)
library(leaflet)
library(magrittr)
library(collapsibleTree)
library(viridis)

####Inputs####
## Dendrogram Setup: 
all_closures <- read.csv("closures_categories0302.csv")
# building colors list
# colfunc <- colorRampPalette(c("royalblue3", "lightsteelblue3", "palegreen4", "greenyellow", "gold"))
# colorslist <- rep(colfunc(5), times = c(3,4,9,48,90))
# sc.g3$reg_type <- as.factor(sc.g3$reg_type)
# sc.g3$gear_type <- as.factor(sc.g3$gear_type)

####Dynamic UI Code####
## Make dynamic ui for method of closure selection:
method.tabs <- tabsetPanel(
  id = "params",
  type = "hidden",
  tabPanel("Closure",
           selectInput("area", "Select closed area(s):",
                       sc.g3$area, selected = NULL, multiple = TRUE)
  ),
  tabPanel("Date", 
           dateRangeInput("dates", label = h5(strong("Date range:")),
                          start=paste0(format(Sys.Date(), "%Y"),'-01-01')),
           selectInput("geartype", "Select gear type:",
                       choices =  c('GILLNET','TRAP/POT','GILLNET & TRAP/POT'), selected = NULL, multiple =FALSE),
           selectInput("region", "Select region(s):",
                       unique(sc.g3$region), selected = NULL, multiple = TRUE)
  ),
  tabPanel("Region",
           selectInput("region.a", "Select region(s):",
                       unique(sc.g3$region), selected = NULL, multiple = TRUE),
           selectInput("geartype.a", "Select gear type:",
                       choices =  c('GILLNET','TRAP/POT','GILLNET & TRAP/POT'), selected = NULL, multiple =FALSE)

  ),
  tabPanel("Regulation",
           selectInput("regtype", "Select regulation type:",
                       sc.g3$reg_type, selected = NULL, multiple = TRUE)
  ))

method.tabs2 <- tabsetPanel(
  id = "params2",
  type = "hidden",
  tabPanel("Gillnet",
           selectInput("regulation", "Color by:",
                       choices = c("Seasonal", "Number of nets"), selected = NULL, multiple = FALSE)
  ),
  tabPanel("TrapPot",
           selectInput("fishery", "Fishery:",
                       choices = c("Northeast Lobster and Jonah crab trap/pot", 
                         "Other trap/pot"), 
                       selected = NULL, multiple = FALSE),
           selectInput("regulation.a", "Color by:",
                       choices = c("Trawl length", "Weak buoy line"),
                       selected = NULL, multiple = FALSE))
)

####UI####------------------------------------------------------------------------------
ui <- 
  fluidPage(
  tags$style("
    body {
    -moz-transform: scale(0.9, 0.9); /* Moz-browsers */
    zoom: 0.9; /* Other non-webkit browsers */
    zoom: 90%; /* Webkit browsers */
}
              "),
    titlePanel(tagList(img(src = 'noaanefsclogo.PNG'),title=''),#br(),title='Decision Support Tool Closures and Regulations App'),
               tags$head(tags$link(rel = "icon", type = "image/png", href = "favicon.png"))
    ),
      tabsetPanel(id = "panels", #panels names the tab panel so it can be referenced for links to panels in ReadMe
        tabPanel("ReadMe",
                 tags$head(tags$style(
                   type="text/css",
                   "#image img {max-width: 100%; width: 100%; height: auto}"
                 )),
                 column(3, br(),
                        img(src = "DSTClosuresAppTitle.png", height=500)),
                 column(9,
                        #Add help text here
                        h3("About the App"),
                        p(" This application was designed to provide background information about the fisheries regulations "
                                 ," that apply to the Decision Support Tool (DST) and development of new regulationes under the"
                                 , a("Altantic Large Whale Take Reduction Plan (ALWTRP)",href="https://www.fisheries.noaa.gov/ALWTRP", 
                                     target="_blank"), "The "
                                 , a("DST", href="https://www.fisheries.noaa.gov/feature-story/decision-support-tool-helpful-those-finding-ways-reduce-whale-entanglement-fishing", 
                                     target="_blank")
                                 ," is designed to aid in the decision-making process involved in reducing mortality"
                                 , " and serious injury in the North Atlantic right whale and other large whales at"
                                 , "brisk of entanglement from fixed gear fisheries."),
                        h3("Using the App"),
                        p("Click the tabs at the top of the page to navigate to various functions within the app. There are three tabs that are described below."
                          ," The interactive maps have zoom functionality and users can click on an area in the map to learn more information about the various"
                          , " restrictions that apply, timing, and any exemptions (if any). " , sep=' '),
                        h4("Closure Map"),
                        p("The "
                          , actionLink("link_to_tabpanel_SCM", "Spatial Closures Map"), "allows the user to query and view various fishing closures on an interactive map."
                          , "The closures included represent the areas that were included in the DST or are newly implemented closures reducing large whale enganglement risk."
                          , "Queries can be performed by a variety of different categories such as a range of dates, region, gear type, regulation type, or the user can"
                          , "simply select from a list of all closures and map closures by name. ", sep=' '),
                        h4("Closure Dendrogram"),
                        p("In addition to the closure map, the ", actionLink("link_to_tabpanel_SCC", "Spatial Closure Categories")
                          , "tab contains a dendrogram that categorizes closures in a stepwise fashion as they apply to either gillnet or trap/pot fisheries"
                          , "and more detailed gear restrictions (gillnet type, mesh size, etc.) and species specific restrictions (lobster pot vs. blue crab etc.) within"
                          , "each spatial closure.", sep=' '),
                        h4("Gear Configuration Map"),
                        p("The", actionLink("link_to_tabpanel_GCM", "Gear Configurations Map"), "lets the user explore relevant gear configuration requirements for"
                          , "trap/pot fisheries (e.g. weak buoy line, trawl length, etc.) and gillnet fisheries (e.g. mesh size, number of nets, etc.) managed under the ALWTRP
                             and the Harbor Porpoise Take Reduction Plan."
                          , "Users can explore area-based requirements according to gear type, fishery (for trap/pot only), and regulation type.", sep=' ')
                        )),
                 
                 
        tabPanel("Spatial Closures Map",
                 tags$div(tags$style(HTML( ".dropdown-menu{z-index:10000 !important;}"))),
                 fluidPage(
                   fluidRow(
                     column(3, 
                       br(),
                       wellPanel(
                         selectInput("method", "Display closed areas by:",
                                     choices = c("Closure","Date","Region","Regulation")),
                                     method.tabs,
                         br(),
                         actionButton("runBtn","SHOW CLOSURES", icon("cogs"), style="color: black; background-color: orange; border-color: grey"),
                       wellPanel(
                          #Add help text here
                         h5(strong("Read Me")),
                         p(  " To begin, select a method for subsetting closures (Ex.",strong("Date, Region, Regulation"),") or choose "
                               , strong("Closure Name"), "to select one or many closures by name "
                               , " to be displayed in the map. Additional options will be displayed for each method and allow further "
                               , " functionality. To display selections on the map click ", strong("SHOW CLOSURES.")
                               , " The interactive map allows the ability to zoom and pan while providing additional information about "
                               , " closure details in a pop-up when clicked. To view the information for each closure, hover your mouse over the area"
                               , " of interest and click. The pop-up for the area highlighted will appear. For overlapping shapes,"
                               , " move the mouse off of the overlapping closures and the next shape below will appear when you bring the cursor back over the"
                               , " shape you are trying to inspect. For a more detailed breakdown of the fishery closures specific "
                               , " to gear, such as gillnet type and mesh size, click the ", actionLink("link_to_tabpanel_SCC2", "Spatial Closure Categories")
                               , " tab.", sep = ''))
                       )),
                     column(9,
                        br(),
                        shinydashboard::box(width = NULL, solidHeader = TRUE, status = 'primary',
                                            leafletOutput('base_map',width="100%",height="80vh")))
                        ))),
        tabPanel("Spatial Closure Categories",
                 br(),
                 collapsibleTreeOutput("plot", height = "500px")),
        tabPanel("Gear Configurations Map",
                 br(),
                 fluidPage(
                   fluidRow(
                     column(3,
                            br(),
                            wellPanel(
                              selectInput("method2", "Select gear type:",
                                          choices =  c('Gillnet','TrapPot')),
                              method.tabs2,
                              actionButton("runBtn2","SHOW AREAS", icon("cogs"),
                                           style="color: black; background-color: green; border-color: grey")),
                             wellPanel(
                       #Add help text here
                       h5(strong("Read Me")),
                       p(  " To begin, select the type of congiguration regulations you wish to view ( Ex.",strong("Gillnet, Trap/Pot "),")."
                           , " Additional options to subset or view regulations by color will be displayed for each method."
                           , " To display selections on the map click ", strong("SHOW AREAS.")
                           , " The interactive map allows the ability to zoom and pan while providing additional information about "
                           , " the name of the area and additional requirements in a pop-up when clicked."
                           , "To view the information for each closure, hover your mouse over the area"
                               , " of interest and click. The pop-up for the area highlighted will appear. For overlapping shapes,"
                               , " move the mouse off of the overlapping closures and the next shape below will appear when you bring the cursor back over the"
                               , " shape you are trying to inspect."))
                     ),
                     column(9,
                            br(),
                            shinydashboard::box(width = NULL, solidHeader = TRUE, status = 'success',
                                                leafletOutput('base_map2',width="100%",height="80vh")))
                   )))
             )
  )

####Server####---------------------------------------------------------------------------
server <- function(input, output, session) {
  
  ####Closure Map Server Code####
  ###### LEAFLET BASE MAP for when app initially loads  
  output$base_map = renderLeaflet({
    # Makes a leaflet map to visualize management areas
    
    leaflet() %>%
      setView(lng = -68.73742, lat = 35, zoom = 5) %>%
      addProviderTiles(providers$Esri.OceanBasemap) %>%
      addScaleBar(position = 'bottomright', options = scaleBarOptions(maxWidth = 250)) 
  })
  
  output$base_map2 = renderLeaflet({
    
    leaflet() %>%
      setView(lng = -68.73742, lat = 35, zoom = 5) %>%
      addProviderTiles(providers$Esri.OceanBasemap) %>%
      addScaleBar(position = 'bottomright', options = scaleBarOptions(maxWidth = 250))
  })
  # Provides link to tabs from ReadMe
  observeEvent(input$link_to_tabpanel_SCM, {
    newvalue <- "Spatial Closures Map"
    updateTabsetPanel(session, "panels", newvalue)
  })
  observeEvent(input$link_to_tabpanel_SCC, {
    newvalue <- "Spatial Closure Categories"
    updateTabsetPanel(session, "panels", newvalue)
  })
  observeEvent(input$link_to_tabpanel_SCC2, {
    newvalue <- "Spatial Closure Categories"
    updateTabsetPanel(session, "panels", newvalue)
  })
  observeEvent(input$link_to_tabpanel_GCM, {
    newvalue <- "Gear Configurations Map"
    updateTabsetPanel(session, "panels", newvalue)
  })
  # Structure for reactive UI described above the ui code
  observeEvent(input$method, {
    updateTabsetPanel(inputId = "params", selected = input$method)
  }) 
    observeEvent(input$runBtn,{
    #clear any previous shapefiles selected
    leafletProxy("base_map") %>%
      clearShapes() %>% clearMarkers %>% clearPopups()  %>% clearControls #clearControls removes the legend so it switches based in input
      sc.g3sub <- 
        switch(input$method,
               Closure = {
                 sc.g3 %>%
                 dplyr::filter(area %in% input$area)
                 },
               Date = {
                 if(input$geartype == 'GILLNET') gearsub <- sc.g3[grep('Gill',sc.g3$gear_type),'shapename']
                 if(input$geartype == 'TRAP/POT') gearsub <- sc.g3[grep('Trap|trap',sc.g3$gear_type),'shapename']
                 if(input$geartype == 'GILLNET & TRAP/POT') gearsub <- sc.g3$shapename
                  
                  jul.range <- as.numeric(format(input$dates, "%j")) #converts shiny input date range into julian days
                  if(jul.range[1]>jul.range[2]){
                    days <- c(seq(1,jul.range[2]),
                      seq(jul.range[1],365))
                  } else {
                    days <- seq(jul.range[1],jul.range[2])
                  }
                  #subset closures by overlap of days
                  ind <- sapply(ClosureDays,function(x) any(days %in% x))
                  namesClosures <- names(ClosureDays[which(ind)])
                  Date = sc.g3 %>%
                    dplyr::filter(region %in% input$region & # by region
                                    shapename %in% namesClosures &
                                    shapename %in% gearsub)
                  },
               Region = {
                 if(input$geartype.a == 'GILLNET') gearsub2 <- sc.g3[grep('Gill',sc.g3$gear_type),'shapename']
                 if(input$geartype.a == 'TRAP/POT') gearsub2 <- sc.g3[grep('Trap|trap',sc.g3$gear_type),'shapename']
                 if(input$geartype.a == 'GILLNET & TRAP/POT') gearsub2 <- sc.g3$shapename
                 Region =  sc.g3 %>%
                   dplyr::filter(region %in% input$region.a & # by region
                                   shapename %in% gearsub2)
                 },
               Regulation = {
                 sc.g3 %>%
                   dplyr::filter(reg_type %in% input$regtype)
               }
        )
      
      #add a palette for the shapefiles and show the legend on the base map

      # if(input$method == "Closure") pal  <- sc.g3sub$reg_type
      # if(input$method == "Date") pal <- sc.g3sub$reg_type
      # if(input$method == "Region") pal  <- sc.g3sub$reg_type
      # if(input$method == "Regulation") pal  <- sc.g3sub$gear_type

pal <- colorFactor(palette = viridis_pal(option='inferno')(6), domain = c("Sea Turtles", "Marine Mammal", 
                                                     "New Marine Mammal", "Fishery", "Habitat", "State"))
      
    #Plot Leaflet Map shapefiles in a loop
   for(k in sc.g3sub$shapename){
      leafletProxy("base_map") %>%
       
        addPolygons(data = shapes[[sc.g3$shapefile[sc.g3$shapename==k]]],
                           stroke = TRUE, color = 
                      # ~colorFactor(palette = viridis_pal(option='inferno')(length(unique(pal))),
                      #    domain = unique(pal))(pal[sc.g3sub$shapename==l]),
                      ~pal(sc.g3sub$reg_type[sc.g3sub$shapename==k]), 
                           opacity = 0.5,
                           weight = 0.5, #fillColor = ~pal(reg_type), 
                          fillOpacity = 0.3, #fill=FALSE,
                          popup =  paste("Area Name: ",sc.g3sub$shapefile[sc.g3sub$shapename==k], "<br>",
                                          "Region: ",sc.g3sub$region[sc.g3sub$shapename==k], "<br>",
                                          "Closure period: ",sc.g3sub$closure_period[sc.g3sub$shapename==k], "<br>",
                                          "Gear Type: ",sc.g3sub$gear_type[sc.g3sub$shapename==k], "<br>",
                                          "RegType: ",sc.g3sub$reg_type[sc.g3sub$shapename==k], "<br>",
                                          "Exemption: ",sc.g3sub$exempted_gear_fishery[sc.g3sub$shapename==k]),
                          highlightOptions=highlightOptions(color="black", weight=2, sendToBack=T))#%>%

   }
      # leafletProxy("base_map") %>% addLegend(pal = colorFactor(palette = viridis_pal(option='inferno')(length(unique(pal))), 
      #                                                          domain = unique(pal), opacity = 0.4, values = unique(pal)))
      leafletProxy("base_map") %>% addLegend(pal = pal, opacity = 0.4, values = c("Sea Turtles", "Marine Mammal", 
                                                                                        "New Marine Mammal", "Fishery", "Habitat", "State"))
      })
    
  ####Dendrogram Server Code####
    #building dendrogram
  output$plot <- renderCollapsibleTree({
    collapsibleTree(
    all_closures,
    hierarchy = c("GearType", "Method", "Mesh", "Closures"),
    root = "All Closures", 
    attribute = "leafCount", tooltip=F, 
    linkLength = 200,
    width = 1400, height = 700, fontSize = 12,
    #fill = colorslist,
    fillByLevel = TRUE
  )
})

  ####Gear Configuration Server Code####
observeEvent(input$method2, {
  updateTabsetPanel(inputId = "params2", selected = input$method2)
})

observeEvent(input$runBtn2,{

  gc1sub <- switch(input$method2,
                   Gillnet = {
                     gc1.a = droplevels(gc1[grepl("gill",gc1$gear_type),])
                   },
                   TrapPot = {
                    if(input$fishery == "Northeast Lobster and Jonah crab trap/pot") trapsub <- droplevels(gc1[grepl("lobster",gc1$fishery) & 
                                                                 grepl("trap",gc1$gear_type),])
                   if(input$fishery == "Other trap/pot") trapsub <- droplevels(gc1[grepl("other", gc1$fishery) & 
                                                grepl("trap",gc1$gear_type),])
                     gc1.a = trapsub
                   }
  )
  
  # if(input$regulation == "Seasonal") col.by  <- gc1sub$seasonal
  # if(input$regulation == "Number of nets") col.by <- gc1sub$min_string_length
  # if(input$regulation.a == "Trawl length") col.by  <- gc1sub$min_string_length
  # if(input$regulation.a == "Weak buoy line") col.by <- gc1sub$max_buoy_line_strength

  col.by <- switch(input$method2,
               'Gillnet' = {
                   if(input$regulation == "Seasonal") gillcol  <- gc1sub$seasonal
                   if(input$regulation == "Number of nets") gillcol <- gc1sub$min_string_length
                   col.a = gillcol
               },
               'TrapPot' = {
                   if(input$regulation.a == "Trawl length") trapcol  <- gc1sub$min_string_length
                   if(input$regulation.a == "Weak buoy line") trapcol <- gc1sub$max_buoy_line_strength
                   col.a = trapcol
               }
               )

  print(head(gc1sub))
  print(col.by)
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
                                 "Length Equivalency: ",gc1sub$string_length_equivalency[gc1sub$shapename==l], "<br>",
                                 "Maximum Length: ",gc1sub$max_string_length[gc1sub$shapename==l], "<br>",
                                 "Minimum Weak Link Strength: ",gc1sub$max_weak_link_strength[gc1sub$shapename==l], "<br>",
                                 "Maximum Line Strength: ",gc1sub$max_buoy_line_strength[gc1sub$shapename==l], "<br>",
                                 "Twine Size (gillnet): ",gc1sub$twine_size[gc1sub$shapename==l], "<br>",
                                 "Tie-downs (gillnet): ",gc1sub$tie_downs[gc1sub$shapename==l], "<br>"),
                  highlightOptions=highlightOptions(color="black", weight=2, sendToBack=T))
    
  }                            
  leafletProxy("base_map2") %>% addLegend(pal = colorFactor(palette = rainbow(length(unique(col.by))),
                                                            domain = unique(col.by)), opacity = 0.4, values = unique(col.by))
})


}

shinyApp(ui, server)
