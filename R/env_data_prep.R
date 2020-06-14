# Prepping environment

libs <- c("sf", "tidyverse")
lapply(libs, library, character.only=T)


# directory structure
dirs<- list()
dirs$data_dir <- "data"
dirs$raw_dir <- file.path(dirs$data_dir, "raw")
dirs$raw_dir_mtbs <- file.path(dirs$raw_dir, "mtbs")

lapply(dirs, dir.create)

# urls
mtbs_url <- "https://edcintl.cr.usgs.gov/downloads/sciweb1/shared/MTBS_Fire/data/composite_data/burned_area_extent_shapefile/mtbs_perimeter_data.zip"

# projections
proj_modis <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"


# downloading mtbs data
if (!exists('mtbs_fire')) {
  mtbs_shp <- file.path(dirs$raw_dir_mtbs,'mtbs_perimeter_data/mtbs_perims_DD.shp')
  if (!file.exists(mtbs_shp)) {
    mtbs_zip <- file.path(dirs$raw_dir_mtbs, 'mtbs_perims_DD.zip')
    download.file(mtbs_url,
                  mtbs_zip)
    unzip(mtbs_zip,exdir = dirs$raw_dir_mtbs)
    unlink(mtbs_zip)
  }
  
  mtbs_fire <- st_read(file.path(dirs$raw_dir_mtbs, "mtbs_perimeter_data"))%>%
    filter(Year >= '2001') %>%
    mutate(discovery_date = ymd(paste(Year, StartMonth, StartDay, sep="-")),
           discovery_year = year(discovery_date),
           discovery_day = day(discovery_date),
           discovery_month = month(discovery_date),
           discovery_doy = yday(discovery_date)) %>%
    rename_all(tolower) %>%
    # Below we are categorizing the fires as in the east or west based on the -97th parallel - which is what MTBS uses
    st_transform("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") %>%
    mutate(lat = st_coordinates(st_centroid(.))[1]) %>%
    mutate(mtbs_region = ifelse(lat < -97, 'West', 'East')) %>%
    dplyr::select(-lat) 
}


