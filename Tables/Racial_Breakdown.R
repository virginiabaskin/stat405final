source("shiny_plot_setup.R")
# Question 1
htx_stats <- read.csv("Datasets/Census Data Houston.csv")
htx_filtered <- filter(htx_stats, `Fact` %in% c("White alone, percent","Black or African American alone, percent", "American Indian and Alaska Native alone, percent", "Asian alone, percent", "Native Hawaiian and Other Pacific Islander alone, percent","Two or More Races, percent"))
htx_filtered <- data.frame(htx_filtered$Fact, htx_filtered$Houston.city..Texas)
htx_filtered <- htx_filtered %>% rename(race = htx_filtered.Fact, population = htx_filtered.Houston.city..Texas)
htx_filtered <- htx_filtered %>%
  mutate(pop = as.numeric(sub("%", "", population))/100)  %>%
  select(-population)

aapi <- htx_filtered %>%filter(`race` %in% c("Asian alone, percent", "Native Hawaiian and Other Pacific Islander alone, percent"))%>% summarize(race = "asian/pacific islander", pop = sum(`pop`))
htx_filtered <- htx_filtered %>% filter(!race %in% c("Asian alone, percent","Native Hawaiian and Other Pacific Islander alone, percent"))
htx_filtered <- bind_rows(htx_filtered, aapi)
htx_pop <- htx_filtered %>% mutate(race = race %>% recode("White alone, percent" = "white", "Black or African American alone, percent" = "black", "American Indian and Alaska Native alone, percent" = "american indian/alaska native", "Two or More Races, percent" = "other"))
counts <- data.frame(na.omit(table(mini_htx$subject_race)))
counts$Freq <- counts$Freq/sum(counts$Freq)
counts <- counts %>% rename(race = Var1, citation_prop = Freq)
htx <- inner_join(htx_pop, counts, by = "race")

