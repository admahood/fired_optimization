# parallel implementation of writing out a firedpy-created .csv to polygons

# this specific application is for an iteration of firedpy for north america
library(dplyr)
library(tidyverse)
library(sf)
library(raster)
library(foreach)
library(doParallel)

# path to a random MCD64 file
# system("aws s3 cp s3://earthlab-natem/modis-burned-area/MCD64A1/C6/archive_derived_tifs/tif_months/BurnDate_A2000306_h08v04.tif data/template.tif")
template_path <- "template/template.tif"



# path to the output .csv from firedpy
# default path in firedpy/proj/outputs/tables
# system("aws s3 cp s3://earthlab-amahood/na_files/tables/fired_events_s1_t5_2020153.csv data/na_fired.csv")
raw_events_file <- "data/na_fired.csv"

# come up with a descriptive name
output_fn <- "data/fired_NorthAmerica_s1_t5.gpkg"

# only requirement here is native modis projection (sinusiodal, 463.something resolution)
# this is for changing everything into the same projection
template <- raster(template_path)


# loading in fire event data frame -- doesn't take too long
df <- read_csv(raw_events_file) %>%
  dplyr::select(id,date,x,y) %>%
  #centering the pixels on the raster cells
  mutate(x = x + (res(template)[1]/2),
         y = y - (res(template)[1]/2),
         year = as.numeric(substr(date, 1,4))) %>%
  # removing about 8000 repeat pixels (i.e. adjacent month detections)
  distinct(x,y,id, .keep_all = T)

# parallel part ================================================================

doParallel::registerDoParallel(parallel::detectCores())

ids <- unique(df$id)

df_poly <- foreach(i = ids, .combine = rbind) %dopar% {
  
  # t0 <- Sys.time() # 15 min for creation, 30 secs to write
  x<- df %>%
    filter(id == i) %>%
    st_as_sf(coords = c("x","y"), crs = crs(template, asText=TRUE)) %>%
    st_buffer(dist = 1+(res(template)[1]/2), endCapStyle = "SQUARE")%>%
    dplyr::summarize()
  
  system(paste("echo", i, which(ids==i), "of", length(ids)))
  
  return(x)

}

st_write(df_poly, output_fn, delete_dsn=TRUE)
system(paste0("aws s3 cp ",
              output_fn,
              " s3://earthlab-amahood/", output_fn))

