#read in google sheets Spatial Closure
library(magrittr)
library(sf)
library(dplyr)

gc <- googlesheets4::read_sheet('https://docs.google.com/spreadsheets/d/1GqDY4458WD49Ub1AXrcswr32R8GNbuYMYhp0HVXLg5o/edit?usp=sharing',
                                 sheet ="GearConfig")
gc <- janitor::clean_names(gc) #probably unnecessary
gc$seasonal <- as.factor(gc$seasonal)
gc$min_string_length <- as.factor(gc$min_string_length)
gc$min_string_length <- factor(gc$min_string_length, levels = c("No Minimum", "2 per buoy line", "3 per buoy line", "5 per buoy line",
                                                                "8 per two buoy lines", "10 per two buoy lines", "15 per two buoy lines",
                                                                "15 Mar-Oct, 20 Nov-Feb per two buoy lines", "20 per two buoy lines",
                                                                "25 per two buoy lines", "35 per two buoy lines", "45 per two buoy lines",
                                                                "50 per two buoy lines", "No ALWTRT Requirements", 
                                                                "Mesh under 7 in: 10, 7 in or more: 16 nets per string",
                                                                "Mesh under 7 in: 10, 7 in or more: 13 nets per string"))

gc$max_string_length <- as.factor(gc$max_string_length)
gc$max_buoy_line_strength <- as.factor(gc$max_buoy_line_strength)
gc$max_buoy_line_strength <- factor(gc$max_buoy_line_strength, levels = c("2,200 lbs",
                                    "1,700 lb weak insertion at 33 percent from the top of the buoy line", 
                                    "1,700 lb weak insertion 50 percent from top of the buoy line",
                                    "1,700 lb weak insertions at 25 and 50 percent of the buoy line length from top",
                                    "1,700 lb weak line for the top 75 percent of one buoy line",
                                    "1,700 lb weak insertions every 60 ft (or full weak line) in top 75 percent of line",
                                    "1,500 lbs",
                                    "No maximum"))

gc1 <- gc %>%
  dplyr::filter(!is.na(shapename))


shapes2 <- list()
#load all shapefiles from 
shape.folder <- ('//net.nefsc.noaa.gov/work4/LobsterGroup/Management/RightWhales/DecisionSupportTool/TempShapefiles/')
for (l in unique(gc1$shapename)){
  shapes2[[l]] <- sf::st_read(paste0(shape.folder,l,'.shp')) %>%
    sf::st_transform('+proj=longlat +datum=WGS84')
    # sf::st_set_crs(4326) %>%
    # sf::st_transform(3857)
}


save.image("TPData.RData")

