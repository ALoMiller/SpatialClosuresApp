#read in google sheets Spatial Closure
library(magrittr)
library(googlesheets4)

#If you have trouble getting access, use the following code and select 0 to get a new token - make sure to check the third box:
#gs4_auth()

sc.g <- googlesheets4::read_sheet('https://docs.google.com/spreadsheets/d/1GqDY4458WD49Ub1AXrcswr32R8GNbuYMYhp0HVXLg5o/edit?usp=sharing',
                                  skip=1)
sc.g <- janitor::clean_names(sc.g)
sc.g1 <- sc.g %>%
  dplyr::filter(in_shapefiles_for_dst_team_2021folder=='Y')
shapes <- list()
#load all shapefiles from 
shape.folder <- ('//net.nefsc.noaa.gov/work4/LobsterGroup/Management/RightWhales/DecisionSupportTool/TempShapefiles/')
for (i in unique(sc.g1$shapefile)){
  shapes[[i]] <- sf::st_read(paste0(shape.folder,i,'.shp')) %>%
                  sf::st_transform('+proj=longlat +datum=WGS84')
                               # sf::st_set_crs(4326) %>%
                               # sf::st_transform(3857)
}  
sc.g2 <- splitstackshape::cSplit(sc.g1, 'day_of_the_year_range', sep=c("\n"), drop=F)
sc.g2$n_times <- stringr::str_count(sc.g2$day_of_the_year_range,'-')
sc.g2 <- splitstackshape::cSplit(sc.g2, 'day_of_the_year_range_1', sep=c("-"), drop=F)
sc.g2 <- splitstackshape::cSplit(sc.g2, 'day_of_the_year_range_2', sep=c("-"), drop=F)
#sc.g2 <- splitstackshape::cSplit(sc.g2, 'day_of_the_year_range_3', sep=c("-"), drop=F)

sc.g3 <- sc.g2 %>% 
  dplyr::group_by(shapefile) %>% 
  dplyr::mutate(shapename=paste0(shapefile,seq(dplyr::n()))) %>%
  as.data.frame()

ClosureDays <- list()
for(j in sc.g3$shapename){
#1 year ranges
if(is.na(sc.g3$day_of_the_year_range_2[sc.g3$shapename==j])){
  if(sc.g3$day_of_the_year_range_1_1[sc.g3$shapename==j]>sc.g3$day_of_the_year_range_1_2[sc.g3$shapename==j]){
    ClosureDays[[j]] <- c(seq(1,sc.g3$day_of_the_year_range_1_2[sc.g3$shapename==j]),
                               seq(sc.g3$day_of_the_year_range_1_1[sc.g3$shapename==j],365))
  } else {
    ClosureDays[[j]] <- seq(sc.g3$day_of_the_year_range_1_1[sc.g3$shapename==j],sc.g3$day_of_the_year_range_1_2[sc.g3$shapename==j])
  }
}
#2 year ranges
if(!is.na(sc.g3$day_of_the_year_range_2[sc.g3$shapename==j])){
  if(sc.g3$day_of_the_year_range_1_1[sc.g3$shapename==j]>sc.g3$day_of_the_year_range_1_2[sc.g3$shapename==j]){
    r1 <- c(seq(1,sc.g3$day_of_the_year_range_1_2[sc.g3$shapename==j]),
            seq(sc.g3$day_of_the_year_range_1_1[sc.g3$shapename==j],365))
  } else {
    r1 <- seq(sc.g3$day_of_the_year_range_1_1[sc.g3$shapename==j],sc.g3$day_of_the_year_range_1_2[sc.g3$shapename==j])
  }
  if(sc.g3$day_of_the_year_range_2_1[sc.g3$shapename==j]>sc.g3$day_of_the_year_range_2_2[sc.g3$shapename==j]){
    r2 <- c(seq(1,sc.g3$day_of_the_year_range_2_2[sc.g3$shapename==j]),
            seq(sc.g3$day_of_the_year_range_2_1[sc.g3$shapename==j],365))
  } else {
    r2 <- seq(sc.g3$day_of_the_year_range_2_1[sc.g3$shapename==j],sc.g3$day_of_the_year_range_2_2[sc.g3$shapename==j])
  }
  ClosureDays[[j]] <- c(r1,r2)
}
}

save.image("PreFormattedData.RData")

