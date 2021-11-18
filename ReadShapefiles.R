#read in google sheets Spatial Closure
library(magrittr)
sc.g <- googlesheets4::read_sheet('https://docs.google.com/spreadsheets/d/1GqDY4458WD49Ub1AXrcswr32R8GNbuYMYhp0HVXLg5o/edit#gid=1463226',
                                  skip=1)
sc.g <- janitor::clean_names(sc.g)
sc.g1 <- sc.g %>%
  dplyr::filter(in_shapefiles_for_dst_team_2021folder=='Y')
shapes <- list()
shape.folder <- ('//net.nefsc.noaa.gov/work4/LobsterGroup/Management/RightWhales/DecisionSupportTool/TempShapefiles/')
for (i in unique(sc.g1$shapefile)){
  shapes[[i]] <- sf::st_read(paste0(shape.folder,i,'.shp'))
}  
sc.g2 <- splitstackshape::cSplit(sc.g1, 'day_of_the_year_range', sep=c("\n"), drop=F)
sc.g2$n_times <- stringr::str_count(sc.g2$day_of_the_year_range,'-')
sc.g2 <- splitstackshape::cSplit(sc.g2, 'day_of_the_year_range_1', sep=c("-"), drop=F)
sc.g2 <- splitstackshape::cSplit(sc.g2, 'day_of_the_year_range_2', sep=c("-"), drop=F)
sc.g2 <- splitstackshape::cSplit(sc.g2, 'day_of_the_year_range_3', sep=c("-"), drop=F)
save.image("PreFormattedData.RData")
