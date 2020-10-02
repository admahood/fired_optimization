library(tidyverse)
library(sf)

x<- st_read("/home/a/Desktop/fired_SouthAmerica_s1_t5.gpkg")

y <- st_centroid(x)

z<-y %>%  st_transform(crs=4326)

z <- z %>%
  mutate(long = st_coordinates(.)[,1],
         lat = st_coordinates(.)[,2]) %>%
  st_set_geometry(NULL)

write_csv(z,"sa_john.csv")
