# rim fire comparison figure
library(tidyverse)
library(sf)
library(ggpubr)
library(ggsn)

# these files are on the guggenheim computer, need to add to project directory

#add in mtbs
FIRED_p <- st_read(file.path("data", "rim_event.gpkg"))
gfa_p <- st_read(file.path("data", "rim_gfa.gpkg")) %>%
  st_transform(crs = st_crs(FIRED_p))
FIRED_daily <- st_read(file.path("data", "rim_daily.gpkg")) %>%
  filter(id == 57206)
mtbs_rim <- mtbs_fire %>%
  filter(fire_name == "RIM", discovery_year == 2013) %>%
  st_transform(crs=st_crs(FIRED_p))
                      

ignition <- FIRED_p %>%
  st_set_geometry(NULL) %>%
  dplyr::select(lat=ignition_latitude, long=ignition_longitude) %>%
  st_as_sf(coords = c("long","lat"), 
           crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") %>%
  st_transform(crs = st_crs(FIRED_p))

bb <- st_bbox(FIRED_p)

as.data.frame(sf::st_coordinates(ignition))-> ig

north <- data.frame(x = c(-120.3, -120.3), y = c(38.05,38.08)) %>%
  st_as_sf(coords = c("x","y"), crs=4326) %>%
  st_transform(crs = st_crs(FIRED_p)) %>%
  st_coordinates() %>%
  as.data.frame()
          
ggarrange(
  ggplot()+
    geom_sf(data = FIRED_p, aes(fill = as.factor(id))) +
    geom_sf(data = ignition, shape = 8, size = 4, stroke = 1.5) +
    scale_fill_discrete(name = "Fire ID")+
   
    theme_bw() + 
    theme(legend.position = c(0.98,0),
          legend.justification = c(1,0),
          plot.title = element_text(face = "bold"),
          legend.background = element_blank(),
          axis.title = element_blank())+
    geom_segment(data = FIRED_p,x=north$X[1], xend = north$X[2], 
                 y=north$Y[1], yend = north$Y[2], 
                 arrow = arrow(type = "closed", angle = 15,
                               length = unit(0.65, "inches")), 
                 lwd = 2, color = "grey30")+
    geom_text(data = FIRED_p,x=north$X[1]+800, y = north$Y[1]+800, label = "N",
              fontface="bold", color = "white", angle = -52, size = 6.5)+
    geom_label(data = ig, 
               x=ig$X+2000,y=ig$Y, label = "Estimated Ingition", fontface = "bold",
               alpha = 0.5, hjust = "left")+
    scalebar(FIRED_p, dist = 5, dist_unit = "km",transform = FALSE, 
             st.dist = .05, st.color = "grey30", box.color = "grey30", 
             box.fill = c("grey30", "white"), st.bottom = TRUE,
             location = "topleft")+
    ggtitle("A: Rim Fire: FIRED"), 
  
  ggplot(FIRED_p)+
    geom_sf(data = gfa_p, aes(fill = as.factor(fire_ID))) +
    scale_fill_discrete(name = "Fire ID") +
    theme_bw() +
    theme(legend.position = c(0.98,0),
          legend.justification = c(1,0),
          plot.title = element_text(face = "bold"),
          legend.background = element_blank(),
          axis.title = element_blank())+
    ggtitle("B: Rim Fire: Global Fire Atlas"),
  ggplot(FIRED_p)+
    geom_sf(data = FIRED_daily, aes(fill = date)) +
    #colors from viridis::viridis(2)
    scale_fill_date(name = "Burn Date",low =  "#FDE725FF", high = "#440154FF") +
    theme_bw() +
    theme(legend.position = c(0.98,0),
          legend.justification = c(1,0),
          legend.background = element_blank(),
          plot.title = element_text(face = "bold"),
          axis.title = element_blank())+
    ggtitle("C: Rim Fire: FIRED daily"),
  ggplot(FIRED_p)+
    geom_sf(data = mtbs_rim, aes(fill = fire_name)) +
    theme_bw() +
    theme(legend.position = c(0.98,0),
          legend.justification = c(1,0),
          plot.title = element_text(face = "bold"),
          legend.background = element_blank())+
    theme(axis.title = element_blank())+
    # xlim(c(bb[1], bb[3])) +
    # ylim(c(bb[2], bb[4])) +
    
    ggtitle("D: Rim Fire: MTBS"),
  ncol =2, nrow=2) +
  ggsave("results/draft_figures/figure_4_rim_fire_comparison.png", dpi = 600, height = 11, width = 16) +
  ggsave("results/draft_figures/figure_4_rim_fire_comparison_small.png", 
         dpi = 300, height = 11, width = 16, limitsize = TRUE)

