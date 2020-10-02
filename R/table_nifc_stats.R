# getting nifc stats for FIRED

library(XML)
library(tidyverse)
library(RCurl)
URL <- getURL("https://www.nifc.gov/fireInfo/fireInfo_stats_totalFires.html")
rt <- readHTMLTable(URL, as.data.frame = T)
rt <- rt[[1]][3:50,]
colnames(rt) <- c("year", "n", "acres")
rt<- as_tibble(rt) %>%
  mutate(year = as.numeric(as.character(year)),
         n = str_replace_all(n,",", "") %>% as.numeric,
         acres = str_replace_all(acres, "\\*", ""),
         acres = str_replace_all(acres, ",", "") %>% as.numeric,
         km2 = acres * .0040468564224) %>%
  filter(year>2000 & year<2017)
rt
sum(rt$km2)
sum(rt$n)
