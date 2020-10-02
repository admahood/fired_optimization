# ecoregion figure

library(tidyverse);library(sf);library(ggpubr)

ecos <- st_read("/home/a/data/background/ecoregions/cec/us_eco_l3.shp") %>%
  mutate(`Level 1 Ecoregion` = str_to_title(NA_L1NAME)) %>%
  group_by(`Level 1 Ecoregion`) %>%
  summarise() %>%
  ungroup()

ecos3 <- st_read("/home/a/data/background/ecoregions/cec/us_eco_l3.shp") %>%
  mutate(`Level 3 Ecoregion` = str_to_title(US_L3NAME))%>%
  dplyr::rename(`Level 1 Ecoregion`=NA_L1NAME)
  
p1<-ggplot(ecos) +
  geom_sf(aes(fill = `Level 1 Ecoregion`)) +
  theme_void() +
  ggtitle("   A. Level 1 Ecoregions")

p2<-ggplot(ecos3) +
  geom_sf(aes(fill=`Level 1 Ecoregion`)) +
  theme(legend.position="none") +
  theme_void()+
  ggtitle("   B. Level 3 Ecoregions")

ggarrange(p1, p2,ncol=1, nrow=2, 
          common.legend = TRUE,legend = "right") +
  ggsave("results/draft_figures/figure_s1_ecos.png", 
         width=7.5, height=9)
