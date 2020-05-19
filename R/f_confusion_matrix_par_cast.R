# this script generates statistics for each space-time combination
# from creating fire event vector objects from MODIS MCD64 raster data
# Author: Adam Mahood

# the general process:
# 1. break up mtbs multipolygons to eliminate the confounding effect of fire
#    complexes sometimes being classified as multiple fires, and sometimes being
#    grouped into a multipolygon
# 2. extract stats on modis pixels that occur within each polygon. the idea is 
#    seeing if one mtbs event corresponds to one modis event




# prepping the environment and adding some libraries and directories -----------
source("R/env_data_prep.R")
library(units)

# create function to extract those from the file names
space <- 5 
time <- 11

years <- 2001:2017 # only up to 2015 because MTBS only goes that far


dir.create("data/yearly_composites_15x15")
dir.create("data/long_tables/")
dir.create("data/result_tables/")
corz = detectCores()-1
registerDoParallel(corz)

# 

# system("aws s3 sync s3://earthlab-natem/modis-burned-area/MCD64A1/C6/result_tables_casted data/result_tables")
# system("aws s3 sync s3://earthlab-natem/modis-burned-area/MCD64A1/C6/long_tables_casted data/long_tables")

# getting state boundaries from us census stored on S3
# system("aws s3 cp s3://earthlab-natem/data/raw/states/cb_2016_us_state_20m.zip data/states/states.zip")
# unzip("data/states/states.zip", exdir="data/states/")

# usa <- st_read(file.path("data/states/", "cb_2016_us_state_20m.shp"),
#                quiet= TRUE) %>%
#   filter(!(STUSPS %in% c("AK", "HI", "PR"))) %>%
#   dplyr::select(STUSPS) %>%
#   st_transform(p4string_ea)
# names(usa) %>% tolower()

# big parallel loop that does everything ---------------------------------------

# setting ss and tt for testing purposes
SS<-5
TT<-11
library(foreach)
foreach(TT = time) %:% # this makes it do nested loops in parallel
  foreach(SS = space)%dopar% {
    
    # import files =============================================================
    modis_full <- read_csv("data/modis_events_alaska.csv") %>%
      mutate(year = as.numeric(substr(date, 1,4))) %>%
      distinct(x,y,id, .keep_all = T)%>%
      st_as_sf(coords = c("x","y"), crs = st_crs(proj_modis))
    
    if(!exists("mtbs")){ 
      mtbs <- mtbs_fire %>%
        st_transform(crs = proj_modis)%>%
        st_cast(to = "MULTIPOLYGON") %>%
        st_cast(to = "POLYGON")
      mtbs$duped <- duplicated(mtbs$fire_id)
      
      mtbs$new_id <- ifelse(mtbs$duped == TRUE,
                            paste(as.character(mtbs$fire_id),
                                  as.character(row_number(mtbs$fire_id)), 
                                  sep="_"),
                            as.character(mtbs$fire_id))
      
      mtbs$cast_area_ha <- st_area(mtbs[0])%>% set_units(value = hectare)
      mtbs$cast_area_ac <- st_area(mtbs[0])%>% set_units(value = acre)
    }
    
    # final result table filename
    bt_fn <- paste0("big_table_s", SS,"t",TT,".csv") 
    
    res_file <-paste0("mtbs_modis_ids_ba_cast_s",SS,"t",TT,".csv") 
    if(!file.exists(paste0("data/result_tables/",res_file))){ 

      results <- data.frame(fire_id = NA, # this is the original mtbs ID
                            mtbs_cast_id = NA, # this is the modified ID in the case of multipolygons
                            modis_id = NA, 
                            n = NA, # number of modis ids within the mtbs polygon
                            mtbs_acres = NA,
                            mtbs_hectares = NA,
                            mtbs_year = NA,
                            modis_ha = NA,
                            modis_acres = NA,
                            west_or_east = NA) 
      
      counter <- 1
      for(y in 1:length(years)){
        # getting the data =====================================================
        modis_y <- modis_full %>%
          dplyr::select(id,date,x,y) %>%
          filter(year == years[y])
      
        mtbs_y <- mtbs[mtbs$year == years[y],] %>%
          filter(str_sub(fire_id,1,2) == "AK")
        
        # extract information for each mtbs polygon ============================
        for(f in 1:nrow(mtbs_y)){
          fire <- mtbs_y[f,]
          modis_in_fire<-modis_y[rowSums(st_intersects(modis_y, 
                                                       fire,sparse = F))>0,]
          
          vc <- unique(modis_in_fire$id)

          if(length(vc) == 0){vc<-NA}
          
          bpix_nobuff <- nrow(modis_in_fire) 
          barea_ha_nobuff <- bpix_nobuff * 21.4369
          barea_ac_nobuff <- bpix_nobuff * 52.9717335
          
          results[counter, 1] <- as.character(fire$fire_id)
          results[counter, 2] <- as.character(fire$new_id)
          results[counter, 3] <- paste(as.character(vc), collapse = " ")
          results[counter, 4] <- length(vc[!is.na(vc)])
          results[counter, 5] <- fire$cast_area_ac
          results[counter, 6] <- fire$cast_area_ha
          results[counter, 7] <- fire$year
          results[counter, 8] <- barea_ha_nobuff
          results[counter, 9] <- barea_ac_nobuff
          results[counter, 10] <- fire$mtbs_region
          print(c(counter, years[y]))
          counter <- counter + 1
        }
      }
      
      
      write_csv(results, paste0("data/result_tables/",res_file))
      # system(paste0("aws s3 cp data/result_tables/",res_file," s3://earthlab-natem/modis-burned-area/MCD64A1/C6/result_tables_casted/",res_file))
      
   }else{results <- read.csv(paste0("data/result_tables/",res_file), stringsAsFactors = FALSE)}
     # breaking it down to just mtbsIDs and modis IDs ------------------
    # here, we're figuring it out from the other direction, how many modis 
    # events don't have an mtbs ID, using the results table we just generated
    # 
    
    longfile = paste0("long_cast_s",SS,"t",TT,".csv")
    if(!file.exists(paste0("data/long_tables/",longfile))){
      long_mt_mo <- data.frame(fire_id=NA, mtbs_cast_id=NA, modis_id=NA)
      counter <- 1
      for(i in 1:nrow(results)){
        ss <- strsplit(results$modis_id[i], " ") %>% unlist()
        if(ss!="NA"){for(j in 1:length(ss)){
           long_mt_mo[counter, 1] <- results$fire_id[i]
           long_mt_mo[counter, 2] <- results$mtbs_cast_id[i]
           long_mt_mo[counter, 3] <- ss[j]
           counter <- counter + 1
         }}else{
           long_mt_mo[counter, 1] <- results$fire_id[i]
           long_mt_mo[counter, 2] <- results$mtbs_cast_id[i]
           long_mt_mo[counter, 3] <- NA
           counter <- counter + 1}
       }
       gc() # doing this as much as possible to conserve resources
       
       
      
       write.csv(long_mt_mo, paste0("data/long_tables/",longfile))
       # system(paste0("aws s3 cp data/long_tables/",
       #               longfile,
       #               " s3://earthlab-natem/modis-burned-area/MCD64A1/C6/long_tables_casted/",
       #               longfile))
    }else{long_mt_mo <- read.csv(paste0("data/long_tables/",longfile), stringsAsFactors = FALSE)}
     # calculate modis id numbers -----------------------------------
     e_th <- 202/21.4369 #thresholds in pixels (hectares)
     w_th <- 404/21.4369
     
     # this is calculating the area of each modis fire event
     m_ids <- data.frame(year = NA, n_ids = NA, n_over_th = NA)
     for(i in 1:length(years)){
       
       modis_y <- modis_full %>%
         filter(year == years[i]) %>%
         st_transform("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") %>%
         mutate(lat = st_coordinates(st_centroid(.))[1]) %>%
         mutate(mtbs_th = ifelse(lat < -97,w_th, e_th)) %>%
         st_set_geometry(NULL)
       
       m_ids[i,1] <- years[i]
       m_ids[i,2] <- modis_y$id %>% unique %>% length
       m_ids[i,3] <- modis_y %>%
         filter(total_pixels>mtbs_th) %>%
         dplyr::select(id) %>%
         pull(id) %>%
         unique() %>%
         length()
     }
     dir.create("data/m_ids/")
     write_csv(m_ids, paste0("data/m_ids/m_ids_s",SS,"t",TT,".csv"))
     # big table time baby ----------------------------------
     
     big_table <- data.frame(modisT_mtbsT = NA,
                             modisF_mtbsT = NA,
                             modisT_mtbsF_all_modis = NA,
                             modisT_mtbsF_modis_over_th = NA,
                             st_combo = NA,
                             mtbs_w_multiple_modis = NA,
                             modis_w_multiple_mtbs = NA,
                             mean_n_modis_per_mtbs = NA,
                             median_n_modis_per_mtbs = NA,
                             max_n_modis_per_mtbs = NA,
                             which_max_modis_per_mtbs = NA,
                             mean_n_mtbs_per_modis = NA,
                             median_n_mtbs_per_modis = NA,
                             max_n_mtbs_per_modis = NA,
                             which_max_mtbs_per_modis = NA,
                             row_check = NA,
                             mtbsT_modisT_unique_modis_events = NA,
                             mtbsT_modisT_total_n_modis_events_with_repeats = NA,
                             space = NA,
                             time = NA,
                             mtbs_IDs_of_max_modis = NA)
     
     rc <- table(results$n) %>% as_tibble(.name_repair = "unique")
     NN <- sum(rc$n[1:2])
     rc <- rc[3:nrow(rc),]
     for(ROW in 1:nrow(rc)){
       XX <- as.numeric(rc$`...1`[ROW])
       NN <- NN + (XX * rc$n[ROW])
     }
     
     n_modis_per_mtbs <- table(long_mt_mo$mtbs_cast_id) %>% 
       as_tibble(.name_repair = "unique") %>%
       filter(`...1` != "NA")
     
     n_mtbs_per_modis <- table(long_mt_mo$modis_id) %>% 
       as_tibble(.name_repair = "unique") %>%
       filter(`...1` != "NA")
     
     big_table[1, 1] <- length(unique(long_mt_mo[!is.na(long_mt_mo$modis_id),]$mtbs_cast_id))
     big_table[1, 2] <- length(unique(long_mt_mo[is.na(long_mt_mo$modis_id),]$mtbs_cast_id))
     big_table[1, 3] <- sum(m_ids$n_ids, na.rm=T) - length(unique(long_mt_mo[is.na(long_mt_mo$modis_id),]$mtbs_cast_id))
     big_table[1, 4] <- sum(m_ids$n_over_th, na.rm=T) - length(unique(long_mt_mo[!is.na(long_mt_mo$modis_id),]$mtbs_cast_id))
     
     big_table[1, 5] <- paste0("s",SS,"t",TT)
     big_table[1, 6] <- nrow(n_modis_per_mtbs[n_modis_per_mtbs$n > 1,])
     big_table[1, 7] <- nrow(n_mtbs_per_modis[n_mtbs_per_modis$n > 1,])
     big_table[1, 8] <- mean(n_modis_per_mtbs$n)
     big_table[1, 9] <- median(n_modis_per_mtbs$n)
     
     max1 <- max(n_modis_per_mtbs$n)
     
     big_table[1, 10] <- max1
     
     big_table[1, 11] <- paste(n_modis_per_mtbs[n_modis_per_mtbs$n == max1,]$`...1`, collapse = " ")
     big_table[1, 12] <- mean(n_mtbs_per_modis$n)
     big_table[1, 13] <- median(n_mtbs_per_modis$n)
     
     max2 <- max(n_mtbs_per_modis$n)
     
     big_table[1,14] <- max2
     
     which2 <- as.numeric(n_mtbs_per_modis[n_mtbs_per_modis$n == max2,]$`...1`)
     
     big_table[1,15] <- paste(as.character(which2), collapse = " ")
     big_table[1,16] <- NN==nrow(long_mt_mo)
     big_table[1,17] <- length(unique(long_mt_mo$modis_id))
     big_table[1,18] <- sum(results$n)
     big_table[1,19] <- SS
     big_table[1,20] <- TT
     big_table[1,21] <- paste(as.character(dplyr::filter(long_mt_mo, modis_id == first(which2))$mtbs_cast_id),collapse = " ")
     
     
     write.csv(big_table, paste0("data/",bt_fn))
     # system(paste0("aws s3 cp data/",bt_fn, 
     #               " s3://earthlab-natem/modis-burned-area/MCD64A1/C6/final_tables_casted/",bt_fn))
     # system(paste0("rm -r ",s3dir))
  }
}


# stiching together the final table ----------------------------------
dir.create("data/final_tables")
system("aws s3 sync s3://earthlab-natem/modis-burned-area/MCD64A1/C6/final_tables_casted data/final_tables")

tables <- list.files("data/final_tables")
table_l <- list()
for(i in 1:length(tables)){
  table_l[[i]] <- read.csv(paste0("data/final_tables/",tables[i]), stringsAsFactors = FALSE)
}

final_table <- do.call("rbind", table_l) %>% as_tibble()
#final_table$modisF_mtbsT<-4223 #whatever... obtained from fixing_confusing_matrix.R
#final_table <- final_table[,-c(1,7:12)]
write.csv(final_table, "data/confusion_matrices.csv")
system("aws s3 cp data/confusion_matrices.csv s3://earthlab-natem/modis-burned-area/MCD64A1/C6/confusion_matrix/confusion_matrices_casted.csv")



# thanks https://gis.stackexchange.com/questions/255025/r-raster-masking-a-raster-by-polygon-also-remove-cells-partially-covered
