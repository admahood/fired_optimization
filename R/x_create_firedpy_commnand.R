# this is an exploratory script I used to extract the tiles needed for alaska.
# shouldn't be necessary once we fix the projection issues

library(sf)
library(tidyverse)
grid <- st_read("/home/a/data/background/modis_grid/arctic.gpkg") 
world <- st_read("/home/a/data/background/world_borders/ne_50m_admin_0_countries.shp") %>%
  st_transform(crs=st_crs(grid))

landtiles<- grid%>%
  st_intersects(world, sparse = F) %>%
  rowSums()

tiles<- grid[landtiles>=1,]

tiles_to_get<- tiles%>% 
  st_set_geometry(NULL) %>%
  dplyr::select(-cat) %>%
  mutate_if(is.numeric,function(x)str_pad(x,width=2, side="left",pad= "0")) %>%
  mutate(tile = paste0("h",h,"v",v)) %>%
  # filter(str_sub(tile, 6,6)!= "3")%>% # option to keep it further north
  pull(tile) %>%
  paste(collapse = " ")

paste("firedpy -tiles", tiles_to_get, 
      "--shapefile -landcover_type 1", "&&",
      "aws s3 cp /home/firedpy/proj/outputs/shapefiles/fired_events_s5_t11_2020122.gpkg s3://earthlab-amahood/fired_events_arctic_h00_to_h03_s5_t11_2020122.gpkg")

# these are the tiles for alaska
# firedpy -tiles h06v03 h07v03 h08v03 h09v03 h10v03 h09v02 h11v03 h10v02 h11v02 h12v02 h13v02 --shapefile -landcover_type 1


# firedpy -tiles h10v03 h11v03 h17v03 h18v03 h19v03 h20v03 h21v03 h22v03 h23v03 h24v03 h25v03 h26v03 h28v03 h10v02 h11v02 h12v02 h13v02 h14v02 h15v02 h16v02 h17v02 h18v02 h19v02 h12v01 h13v01 h14v01 h15v01 h16v01 h17v01 h18v01 h19v01 h16v00 h17v00 h18v00 h09v03 --shapefile -landcover_type 1 &&
  # aws s3 cp /home/firedpy/proj/outputs/shapefiles/fired_events_s5_t11_2020122.gpkg s3://earthlab-amahood/fired_events_arctic_s5_t11_2020122.gpkg