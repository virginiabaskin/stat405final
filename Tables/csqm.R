source("shiny_plot_setup.R")
beats_data <- read_csv("Datasets/COH_POLICE_BEATS.csv")
citations_in_beat <- mini_htx %>% inner_join(beats_data, by="Beats") %>%
  group_by(Beats) %>%
  summarise(n_individuals = n())

citations_in_beat <- as.data.frame(citations_in_beat)

cit_beat_area <- citations_in_beat %>% 
  inner_join(beats_data[,c("Beats", "Area_sq_mi")], by="Beats")

cit_beat_area$Citations_per_sqm <- cit_beat_area$n_individuals / cit_beat_area$Area_sq_mi
csqm <- cit_beat_area[,c("Beats","Citations_per_sqm")]

cqm_summary <- data.frame(matrix(ncol=3,nrow=1))
colnames(cqm_summary) <- c("median","standard deviation", "IQR")
cqm_summary[,1] <- csqm %>% summarise(median(Citations_per_sqm))
cqm_summary[,2] <- csqm %>% summarise(sd(Citations_per_sqm))
cqm_summary[,3] <- csqm %>% summarise(IQR(Citations_per_sqm))

csqm_top <- csqm %>% arrange(desc(Citations_per_sqm)) %>% slice_head(n=5) %>% rename("Citations Per Sq. mi."="Citations_per_sqm")

csqm_bottom <- csqm %>% arrange(desc(Citations_per_sqm)) %>% slice_tail(n=5) %>% rename("Citations Per Sq. mi."="Citations_per_sqm")