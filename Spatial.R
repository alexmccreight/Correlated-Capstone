library(tidycensus)
library(tidyverse)
library(sf)
library(readxl)
options(tigris_use_cache = TRUE)
census_api_key("593dedcd53f3a58724af5ac88fc2916d21a6ed05")



v20 <- load_variables(2020, "acs5", cache = TRUE)


VARS <- c(Income = "B19013_001", Pop = 'B01003_001', Age = "B01002_001")


DC_quant <- get_acs(state = "DC", 
                    geography = "tract", 
                    variables = VARS, 
                    year = 2020,
                    geometry = TRUE, 
                    output='wide')%>% 
  select(-ends_with('M'))  



get_DC_data <- function(V,V1,NAMES){
  DC_cat <- get_acs(state = "DC", geography = "tract", 
                    variables = V, geometry = TRUE, output='wide', summary_var = V1)
  
  DC_cat %>% 
    select(-ends_with('M')) %>%
    mutate(Total = select(.,-c(GEOID,NAME,summary_est,summary_moe)) %>% rowSums(na.rm=TRUE)) %>%
    mutate(across(-c(GEOID,NAME,summary_est,summary_moe), ~.x/DC_cat$summary_est)) %>%
    select(-c(summary_est,summary_moe,Total))
}



V1 <- "B02001_001"
V <- paste0("B02001_0",str_pad(c(2:8),2,"0",side='left'))
NAMES <- v20 %>% filter(name %in% V) %>% pull(label) %>% str_replace('Estimate\\!\\!Total\\:\\!\\!','')

DC_cat <- get_acs(state = "DC", geography = "tract", 
                  variables = V, geometry = FALSE, output='wide', summary_var = V1)
race <- DC_cat %>% 
  select(-ends_with('M')) %>%
  mutate(across(-c(GEOID,NAME,summary_est,summary_moe), ~.x/summary_est)) %>%
  select(-c(summary_est,summary_moe))

names(race) = c('GEOID','NAME',trimws(paste0('Race_',str_sub(NAMES,0,26))))

DC_data <- DC_quant %>% left_join(race)


# colleges <- read_csv("hd2020.csv")
colleges <- read_csv("D:\\STAT 452\\Correlated-Capstone\\hd2020.csv")
colleges <- sf::st_as_sf(colleges,coords = c('LONGITUD','LATITUDE'))
st_crs(colleges) <- DC_data %>% st_crs()

col_coord <- data.frame(st_coordinates(colleges))

colleges_sub <- colleges[col_coord$X > st_bbox(DC_data)$xmin &  col_coord$X < st_bbox(DC_data)$xmax & col_coord$Y > st_bbox(DC_data)$ymin &  col_coord$Y < st_bbox(DC_data)$ymax,]

#Approx 1/2 mile radius
DC_data$NumColleges <- st_intersects(DC_data,st_buffer(colleges_sub,dist=800)) %>% lengths()

# areawater <- read_sf("Waterbodies") 
areawater <- read_sf("D:\\STAT 452\\Correlated-Capstone\\Waterbodies")
areawater <- st_transform(areawater,crs = st_crs(DC_data))
# roads <- read_sf("tl_2018_11001_roads")
roads <- read_sf("D:\\STAT 452\\Correlated-Capstone\\tl_2018_11001_roads")
roads <- st_transform(roads,crs = st_crs(DC_data))

points_of_interest <- read_csv("D:\\STAT 452\\Correlated-Capstone\\Points_of_Interest.csv")
points_of_interest <- points_of_interest %>% filter(ALIASNAME == "WASHINGTON MONUMENT") %>% 
  select(X, Y, ALIASNAME) %>% 
  st_as_sf(coords = c('X','Y')) %>% 
  st_set_crs(st_crs(DC_data)) %>% 
  st_transform()
  

roads_sub <- roads %>% filter( (st_intersects(roads,DC_data) %>% lengths()) > 0) %>% filter(RTTYP %in% c('U','I'))
roads_sub <- st_crop(roads_sub,st_bbox(DC_data))
DC_data$AREA = st_area(DC_data) %>% as.vector() #square feet

# library(crsuggest)
# suggest_crs(DC_data)
#areawater <- st_transform(areawater, 6488)
#colleges_sub <- st_transform(colleges_sub,crs = 6488)
#DC_data <- st_transform(DC_data,crs = 6488)
#roads_sub <- st_transform(roads_sub,crs=6488) 

areawater <- st_make_valid(areawater)
DC_data$DistToRiver = st_distance(DC_data,areawater %>% filter(GIS_ID == 'WaterPly_11')) %>% as.vector()
distToRoads <- st_distance(DC_data,roads_sub) %>% units::drop_units() %>% as.matrix() 

DC_data$MinDistToHwy = distToRoads %>% apply(1,min)
DC_data$NumHwys = distToRoads %>% apply(1,function(v) length(unique(roads_sub$FULLNAME[v == 0])))
DC_data$AnyHwys = DC_data$NumHwys > 0

save(colleges_sub,DC_data,areawater,roads_sub,points_of_interest, file = 'SpatialData.RData')

