library(RSQLite)
library(dplyr)
library(stringr)
library(grid)
library(readr)
library(ggfortify)
library(ggthemes)
options(dplyr.summarise.inform = FALSE)

# import data
# full_htx <- read_csv("/stat405final/Datasets/tx_houston_2023_01_26.csv")
census_csv <- read_csv("Datasets/Census Data Houston.csv")
mini_htx <- read_csv("Datasets/mini_htx.csv")
# mini_htx <- full_htx[seq(1,2000000,3),]
mini_htx <- mini_htx %>% rename("Beats" = "beat")
#beats_data <- read_csv("Datasets/COH_POLICE_BEATS.csv")
#htx_stats <- read_csv("Datasets/DECENNIALPL2020.P2-2023-03-02T224324.csv")

# subset the data to only contain columns of interest
citations_htx <- mini_htx[,c("subject_race", "subject_sex", "district", "date", "violation")]

# subset the data to only contain rows of violation interest
citations_htx <- citations_htx %>% 
  filter(grepl("RED LIGHT", violation)|grepl("STOP SIGN", violation)|
           grepl("SEAT BELT", violation)|grepl("FINANCIAL RESPONSIBILITY", violation)|
           grepl("LICENSE", violation)|grepl("SPEEDING", violation)) %>%
  mutate(date = format(date, format = "%Y"))

#create the circles dataframe which counts individuals in each race, year, district pairing
citations_htx_circle <- citations_htx %>%
  group_by(subject_race, date, district) %>%
  na.omit()%>%
  summarise(n_individuals = n()) 

# create the bars dataframe which counts the number of individuals in each race, year, district, violation, pairing

# create a dataframe with all possible combinations of race, date, sex, and violation
citations_htx_bar <- citations_htx %>%
  mutate(red_stop= if_else(grepl("RED LIGHT", violation)|grepl("STOP SIGN", violation), 1, 0)) %>%
  mutate(seat_belt= if_else(grepl("SEAT BELT", violation), 1, 0)) %>%
  mutate(fin_resp= if_else(grepl("FINANCIAL RESPONSIBILITY", violation), 1, 0))%>%
  mutate(license= if_else(grepl("LICENSE", violation), 1, 0))%>%
  mutate(speeding= if_else(grepl("SPEEDING", violation), 1, 0))%>%
  select(-violation)%>%
  group_by(date, subject_race, district, subject_sex, red_stop, seat_belt, fin_resp, license, speeding)%>%
  summarise(n_individuals = n())

races <- unique(citations_htx_bar$subject_race) # races
years <- unique(citations_htx_bar$date) # dates


barplot <- function(red_stop_m, red_stop_f, seat_belt_m, seat_belt_f, fin_resp_m, fin_resp_f, license_m, license_f, speeding_m, speeding_f){
  y <- c(0.5, 0.5)
  x <- c(0, 1.1)
  # grid.newpage()
  citations <- c("red_stop", "seat_belt", "fin_resp", "license", "speeding")
  male <- c(red_stop_m, seat_belt_m, fin_resp_m, license_m, speeding_m)
  max_male <- max(male)
  female <- c(red_stop_f, seat_belt_f, fin_resp_f, license_f, speeding_f)
  max_female <- max(female)
  colors <- c("lightcyan","lightpink","moccasin","skyblue1","thistle1")
  for (i in 1:5){
    male_scale <-  male[i]/(2*max(male))
    female_scale <- female[i]/(2*max(female))
    grid.rect(x=0.2*i-0.1, y = 0.5+0.5*male_scale, height = male_scale, width = 0.199, gp = gpar(fill = colors[i]))
    grid.rect(x=0.2*i-0.1, y = 0.5-0.5*female_scale, height = female_scale, width = 0.199, gp = gpar(fill = colors[i]))
    #grid.text(citations[i], x = 0.2*i-0.1, y = 0.52)
  }
  grid.lines(x, y, gp = gpar(lwd = 1, col = "black"))
  #grid.text ("M", x = 0.9, y = 0.9)
  #grid.text ("F", x = 0.9, y = 0.1)
}

bar_run <- function(plot_race,plot_year, districts){
  new_df <- citations_htx_bar[which(citations_htx_bar$date == plot_year &
                                      citations_htx_bar$subject_race==plot_race &
                                      citations_htx_bar$district==districts), ]
  male_df <- new_df[which(new_df$subject_sex == "male"),]
  female_df <- new_df[which(new_df$subject_sex == "female"),]
  red_stop_m <- sum(male_df$red_stop == 1)
  red_stop_f <- sum(female_df$red_stop == 1)
  seat_belt_m <- sum(male_df$seat_belt == 1)
  seat_belt_f <- sum(female_df$seat_belt == 1)
  fin_resp_m <- sum(male_df$fin_resp == 1)
  fin_resp_f <- sum(female_df$fin_resp == 1)
  license_m <- sum(male_df$license == 1)
  license_f <- sum(female_df$license == 1)
  speeding_m <- sum(male_df$speeding == 1)
  speeding_f <- sum(female_df$speeding == 1)
  barplot(red_stop_m, red_stop_f, seat_belt_m, seat_belt_f, fin_resp_m, fin_resp_f, license_m, license_f, speeding_m, speeding_f)
}

#DRAW CIRCLE FUNCTION DONE

#given frequency, plot a properly scaled circle in the center of the viewpoint
#max_cite is the citation count of the race, year with the most citations
#found by max(cite_counts_list)
draw_circle <- function(count, max_count = max_cite){
  my_circle <- circleGrob(name = "my_circle",
                          x = 0.5, y = 0.5, r = .8*count/(max_count), #radius scale needs to be fixed and them im done
                          gp = gpar(col = "black", fill = "#BEBEBE66", lty = 1, lwt = 2))
  grid.draw(my_circle)
}

# draw_circle(6, 10)
# popViewport()

legend <- function(citations, colors){
  grid.rect(x = 0, y= 0, just = c("left", "bottom"), width = 1, height = .9)
  grid.text("LEGEND",
            x = .5, y = 0.915, just = c("center", "bottom"),
            gp = gpar(fontsize = 10, fontface = "bold"))
  
  #citations + corresponding colors
  #citations <- c("Red/Stop", "Seat Belt", "Financial", "License", "Speeding")
  #colors <- c("lightcyan","lightpink","moccasin","skyblue1","thistle1")
  ys <- c(0.74, 0.65, 0.56, 0.47, 0.38)
  grid.text("Citation Type", x= 0.05, y= 0.87, just = c("left", "top"), gp = gpar(fontsize=8, fontface="bold"))
  for (i in 1:5){
    grid.rect(x = 0.17, y = ys[i], just = c("left", "bottom"), height = 0.077, width = 0.15, 
              gp = gpar(col = "black", fill=colors[i]))
    grid.text(citations[i], x = 0.37, y = ys[i]+0.038, just = c("left", "center"), gp = gpar(fontsize=7))
  }
  
  grid.text("Citation Frequency", x = 0.04, y = 0.34, just = c("left", "top"), gp = gpar(fontsize=8, fontface="bold"))
  grid.text("Compared to other (race, year)s", x = 0.037, y = 0.3, just = c("left", "top"), gp = gpar(fontsize=5))
  #comparatively less citations for race that year 
  #comparatively more citations
  
  grid.circle(x = 0.15, y = 0.233, r = 0.04, gp = gpar(col = "black", fill = "#BEBEBE66", lty = 1, lwt = 2))
  grid.text("Less Citations", x = 0.3, y = 0.233, just = c("left", "center"), gp = gpar(fontsize=7))
  grid.text("Overall", x = 0.43, y = 0.2, just = c("left", "center"), gp = gpar(fontsize=7))
  
  grid.circle(x = 0.18, y = 0.11, r = 0.175, gp = gpar(col = "black", fill = "#BEBEBE66", lty = 1, lwt = 2))
  grid.text("More Citations", x = 0.4, y = 0.11, just = c("left", "center"), gp = gpar(fontsize=7))
  grid.text("Overall", x = 0.53, y = 0.075, just = c("left", "center"), gp = gpar(fontsize=7))
}
