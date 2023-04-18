source("shiny_plot_setup.R")
speeding_race <- mini_htx %>% filter(grepl("SPEEDING", violation)) %>%
  group_by(subject_race) %>%
  summarise(n_individuals = n()) %>%
  arrange(desc(n_individuals)) %>%
  rename("Subject Race"="subject_race", "Speeding"="n_individuals")

# Invalid License Citations
license_race <- mini_htx %>% filter(grepl("LICENSE", violation)) %>%
  group_by(subject_race) %>%
  summarise(n_individuals = n()) %>%
  arrange(desc(n_individuals))%>%
  rename("Subject Race"="subject_race", "Invalid License"="n_individuals")


# Failure to establish Financial Responsibility Citations
financial_race <- mini_htx %>% filter(grepl("FINANCIAL RESPONSIBILITY", violation)) %>%
  group_by(subject_race) %>%
  summarise(n_individuals = n()) %>%
  arrange(desc(n_individuals))%>%
  rename("Subject Race"="subject_race", "Financial Responsibility"="n_individuals")

# seat belt citations
seat_belt_race <- mini_htx %>% filter(grepl("SEAT BELT", violation)) %>%
  group_by(subject_race) %>%
  summarise(n_individuals = n()) %>%
  arrange(desc(n_individuals))%>%
  rename("Subject Race"="subject_race", "Seat Belt"="n_individuals")


# Running a stop light/red light
stop_light_race <- mini_htx %>% 
  filter(grepl("RED LIGHT", violation)|grepl("STOP SIGN", violation)) %>%
  group_by(subject_race) %>%
  summarise(n_individuals = n()) %>%
  arrange(desc(n_individuals))%>%
  rename("Subject Race"="subject_race", "Stop Sign/Red Light"="n_individuals")

citation_race <- inner_join(speeding_race, license_race, by = "Subject Race")
citation_race <- inner_join(citation_race, financial_race, by = "Subject Race")
citation_race <- inner_join(citation_race, seat_belt_race, by = "Subject Race")
citation_race <- inner_join(citation_race, stop_light_race, by = "Subject Race")

