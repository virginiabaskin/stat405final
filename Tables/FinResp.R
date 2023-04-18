source("shiny_plot_setup.R")

# black
black <- mini_htx[mini_htx$subject_race=='black',]
black_total <- nrow(black)

blk_fin <- black %>% filter(violation=="FAILURE TO ESTABLISH FINANCIAL RESPONSIBILITY") %>%
  summarise(n_individuals = n()) %>%
  arrange(desc(n_individuals))
blk_perc <- blk_fin[1]/black_total

# white
white <- mini_htx[mini_htx$subject_race=='white',]
white_total <- nrow(white)

white_fin <- white %>% filter(violation=="FAILURE TO ESTABLISH FINANCIAL RESPONSIBILITY") %>%
  summarise(n_individuals = n()) %>%
  arrange(desc(n_individuals))
white_perc <- white_fin[1]/white_total

# aapi
aapi <- mini_htx[mini_htx$subject_race=='asian/pacific islander',]
aapi_total <- nrow(aapi)
aapi_fin <- aapi %>% filter(violation=="FAILURE TO ESTABLISH FINANCIAL RESPONSIBILITY") %>%
  summarise(n_individuals = n()) %>%
  arrange(desc(n_individuals))
aapi_perc <- aapi_fin[1]/aapi_total

# unknown
unknown <- mini_htx[mini_htx$subject_race=='unknown',]
unknown_total <- nrow(unknown)

unknown_fin <- unknown %>% filter(violation=="FAILURE TO ESTABLISH FINANCIAL RESPONSIBILITY") %>%
  summarise(n_individuals = n()) %>%
  arrange(desc(n_individuals))
unknown_perc <- unknown_fin[1]/unknown_total

# NA
nas <- mini_htx[is.na(mini_htx$subject_race),]
nas_total <- nrow(unknown)

nas_fin <- nas %>% filter(violation=="FAILURE TO ESTABLISH FINANCIAL RESPONSIBILITY") %>%
  summarise(n_individuals = n()) %>%
  arrange(desc(n_individuals))
nas_perc <- nas_fin[1]/nas_total

all_perc <- data.frame(c(blk_perc, white_perc, aapi_perc, unknown_perc, nas_perc))
colnames(all_perc) <- c("Black", "White", "AAPI", "Unknown","Unrecorded")