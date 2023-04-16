source("shiny_plot_setup.R")
#district scatter

#Scatter plot longitude vs latitude by district
# Verifying the hubs for citation and locations of districts

district_loc <- function(){ 
  mini_htx2 <- mini_htx[seq(1,666666,30),]
  mini_htx2$district <- factor(mini_htx2$district)
  
  plot(mini_htx2$lat, mini_htx2$lng, col = mini_htx2$district, panel.first = grid(8,8),  
       xlim = c(29.5, 30.15), ylim = c(-96, -95), main = "Latitude & Longitude of Stops by District", 
       xlab = "Latitude", ylab = "Longitude", pch=1)
  legend("bottomright", legend=levels(mini_htx2$district), cex = 0.62, pch=1, 
         col=unique(mini_htx2$district), title = "District")
}
