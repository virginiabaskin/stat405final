source("shiny_plot_setup.R")

#frequency of different levels
speed_over_freq <- function(){

  mini_htx2 <- mini_htx[seq(1,666666,30),]
  mini_htx2$district <- factor(mini_htx2$district)
  mini_htx2$diff <- abs(mini_htx2$speed - mini_htx2$posted_speed)
  mini_htx3 <- mini_htx2[, c("date", "diff")]
  mini_htx3 <- na.omit(mini_htx3)
  mini_htx3 <- mini_htx3[mini_htx3$diff != 693,]
  
  plot2 <- ggplot(mini_htx3, aes(date, diff)) + geom_point(size=.5) +
    labs(subtitle="Frequency of Citations for Different Levels of Speeding", 
         y="MPH Speeding", 
         x="Year", 
         title="MPH over Limit for Speeding Citations 2014-2020")
  
  ggMarginal(plot2, type = "histogram", fill="slateblue", margins = 'y')
  #ggMarginal(plot2, type = "boxplot", fill="slateblue", margins = 'y')
  #ggMarginal(plot2, color ="slateblue", margins = 'y')
}