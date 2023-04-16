source("shiny_plot_setup.R")

#ADD COLOR TO THIS PLOT 
# Observe citations over time and any time-associate patterns
time_line <- function(){ 
  time <- as.POSIXct(mini_htx$time, format = "%H:%M:%S")
  hour <- as.numeric(format(time, "%H"))
  count <- data.frame(table(na.omit(hour)))
  plot(count$Var1, count$Freq, type = "n", xlab = "Hour", ylab = "Frequency of Citations", main = "Frequency of Citations vs Hour in Day")
  lines(count$Var1, count$Freq, type = "l", lwd = 2)
}