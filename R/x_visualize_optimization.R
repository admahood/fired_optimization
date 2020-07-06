# visualize results

# getting results tables for each space-time thing
command <- "aws s3 sync"

# results of confusion_matrix_par_cast
from <- "s3://earthlab-amahood/firedpy_optimization/tables/"
to <- "data/tables"

system(paste(command, from, to))

# rbinding the tables (each table is a single row)
files <- list.files("data/tables", full.names = TRUE)

table_list <- list()
for(i in 1:length(files)){
  table_list[[i]] <- read_csv(files[i])
}

final_table <- do.call("rbind", table_list) %>% as_tibble() %>%
mutate(ratio = round(modisT_mtbsT/mtbsT_modisT_unique_modis_events, 4),
       ratios = abs(ratio-1) == min(abs(ratio-1)))

# ratio is what we used for the fired paper to get to s5 t11

breaks <- c(min(final_table$ratio), 1, max(final_table$ratio))

p1 <- ggplot(final_table, aes(x = space, y = time)) +
  scale_color_manual(values = c(NA, "black")) +
  geom_tile(aes(fill = ratio, color = ratios), lwd = 1) +
  scale_fill_gradient2(low = "red", mid = "white", 
                       high = "blue", midpoint = 1, breaks = breaks) 

# different ways of looking at the information extracted with
# f_confusion_matrix_par_cast.R

# x with multiple y ======================
ggplot(final_table, aes(x=space, y=scale(mtbs_w_multiple_modis), 
                        color = as.factor(time)))+
  geom_line() +
  geom_line(aes(y=scale(modis_w_multiple_mtbs)), lty=2) +
  ggtitle("dotted lines are modis events overlapping with multiple mtbs events",
          "(Too much aggregation)")+
  facet_wrap(~time) +
  ylab("MTBS Events containing multiple MODIS events (oversegmentation)")

ggplot(final_table, aes(x=time, y=scale(mtbs_w_multiple_modis), 
                        color = as.factor(space)))+
  geom_line() +
  geom_line(aes(y=scale(modis_w_multiple_mtbs)), lty=2) +
  ggtitle("dotted lines are modis events overlapping with multiple mtbs events",
          "(Too much aggregation)")+
  facet_wrap(~space) +
  ylab("MTBS Events containing multiple MODIS events (oversegmentation)")

# mean counts ==========================
ggplot(final_table, aes(x=space, y = scale(mean_n_mtbs_per_modis), 
                        color = as.factor(time))) +
  geom_line() +
  geom_line(aes(y = scale(mean_n_modis_per_mtbs)), lty=2)+
  ggtitle("dotted lines are mean_n_modis_per_mtbs")+
  facet_wrap(~time)

ggplot(final_table, aes(x=time, y = (mean_n_mtbs_per_modis), 
                        color = as.factor(space))) +
  geom_line() +
  geom_line(aes(y = (mean_n_modis_per_mtbs)), lty=2)+
  ggtitle("dotted lines are mean_n_modis_per_mtbs")+
  facet_wrap(~space)

ggplot(final_table, aes(x=time, y = mean_n_modis_per_mtbs, 
                        color = as.factor(space))) +
  geom_line() +
  geom_hline(yintercept = c(1,2,3), lty=2)

ggplot(final_table, aes(x=space, y = mean_n_modis_per_mtbs, 
                        color = as.factor(time))) +
  geom_line() +
  geom_hline(yintercept = c(1,2,3), lty=2)

ggplot(final_table, aes(x=space, y = mean_n_mtbs_per_modis, 
                        color = as.factor(time))) +
  geom_line() +
  geom_hline(yintercept = c(1), lty=2)

ggplot(final_table, aes(x=time, y = mean_n_mtbs_per_modis, 
                        color = as.factor(space))) +
  geom_line()
