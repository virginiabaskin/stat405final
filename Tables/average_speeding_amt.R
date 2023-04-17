# Question 3
source("shiny_plot_setup.R")
## All people
all_speed <- mutate(mini_htx, diff = speed - posted_speed) %>% 
  filter(posted_speed >= 0) %>% 
  summarise(mean = mean(diff, na.rm = TRUE), 
            median = median(diff, na.rm = TRUE), 
            sd = sd(diff, na.rm = TRUE))
race_speed <- mutate(mini_htx, diff = speed - posted_speed) %>% 
  filter(posted_speed >= 0) %>% group_by(raw_race) %>%
  summarise(total = n(),  ## Function to use internally of summarise only
            mean = mean(diff, na.rm = TRUE),
            median = median(diff, na.rm = TRUE),
            sd = sd(diff, na.rm = TRUE)) 
gender_speed <- mutate(mini_htx, diff = speed - posted_speed) %>% 
  filter(posted_speed >= 0) %>% group_by(subject_sex) %>%
  summarise(total = n(),  
            mean = mean(diff, na.rm = TRUE),
            median = median(diff, na.rm = TRUE),
            sd = sd(diff, na.rm = TRUE)) 
race_gender_speed <- mutate(mini_htx, diff = speed - posted_speed) %>% 
  filter(posted_speed >= 0) %>% group_by(raw_race, subject_sex) %>%
  summarise(total = n(),
            mean = mean(diff, na.rm = TRUE),
            median = median(diff, na.rm = TRUE),
            sd = sd(diff, na.rm = TRUE)) 