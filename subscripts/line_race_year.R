#citation count by race by year line graph
#Race Line Graph by Year: line_ry()

source("shiny_plot_setup.R")

race_date <- mini_htx[,c("date", "subject_race")] # race and date
race_date <- na.omit(race_date) # omit na Values

year_only <- function(fulldate){
  year <- substring(fulldate, 1, 4) # only keep year
}

line_ry <- function(){ 
  black <- race_date[race_date$subject_race=="black",]
  black$year <- unlist(lapply(black$date, year_only))
  plot(sort(unique(black$year)), as.vector(table((as.factor(black$year)))), 
       ylim = c(0, 60000), main = "Citations by Race for Each Year", 
       xlab="Year", ylab = "Count") #200000 when all
  lines(sort(unique(black$year)), as.vector(table((as.factor(black$year)))), 
        type = "l", col = "blue")
  
  white <- race_date[race_date$subject_race=="white",]
  white$year <- unlist(lapply(white$date, year_only))
  points(sort(unique(white$year)), as.vector(table(as.factor(white$year))))
  lines(sort(unique(white$year)), as.vector(table((as.factor(white$year)))), 
        type = "l", col = "red")
  
  aapi <- race_date[race_date$subject_race=="asian/pacific islander",]
  aapi$year <- unlist(lapply(aapi$date, year_only))
  points(sort(unique(aapi$year)), as.vector(table(as.factor(aapi$year))))
  lines(sort(unique(aapi$year)), as.vector(table((as.factor(aapi$year)))), 
        type = "l", col = "green")
  
  
  unknown <- race_date[race_date$subject_race=="unknown",]
  unknown$year <- unlist(lapply(unknown$date, year_only))
  points(sort(unique(unknown$year)), as.vector(table(as.factor(unknown$year))))
  lines(sort(unique(unknown$year)), as.vector(table((as.factor(unknown$year)))), 
        type = "l", col = "orange")
  
  other <- race_date[race_date$subject_race=="other",]
  other$year <- unlist(lapply(other$date, year_only))
  points(sort(unique(other$year)), as.vector(table(as.factor(other$year))))
  lines(sort(unique(other$year)), as.vector(table((as.factor(other$year)))), 
        type = "l", col = "purple")
  
  legend("topright", legend=c("black", "white", "aapi", "unknown", "other"), 
         col=c("blue", "red", "green", "orange", "purple"), pch = 20, 
         cex = 0.7, title = "Race")
}
