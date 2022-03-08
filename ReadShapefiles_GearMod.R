#read in google sheets Spatial Closure
library(magrittr)
library(sf)
library(dplyr)

gc <- googlesheets4::read_sheet('https://docs.google.com/spreadsheets/d/1GqDY4458WD49Ub1AXrcswr32R8GNbuYMYhp0HVXLg5o/edit?usp=sharing',
                                 sheet ="GearConfig")
gc <- janitor::clean_names(gc) #probably unnecessary
gc$seasonal <- as.factor(gc$seasonal)
gc$min_string_length <- as.factor(gc$min_string_length)
gc$max_string_length <- as.factor(gc$max_string_length)
gc$max_buoy_line_strength <- as.factor(gc$max_buoy_line_strength)
gc1 <- gc %>%
  dplyr::filter(!is.na(shapename))
shapes2 <- list()
#load all shapefiles from 
shape.folder <- ('//net.nefsc.noaa.gov/work4/LobsterGroup/Management/RightWhales/DecisionSupportTool/TempShapefiles/')
for (l in unique(gc1$shapename)){
  shapes2[[l]] <- sf::st_read(paste0(shape.folder,l,'.shp')) #%>% 
    #sf::st_set_crs(4326) %>% 
    #sf::st_transform(3857)
}


save.image("TPData.RData")
