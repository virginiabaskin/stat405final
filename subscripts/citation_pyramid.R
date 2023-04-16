#citation pyramid 
source("shiny_plot_setup.R")

#x axis citation number, y axis is district, color is gender
pyramid_cites <- function(){
  options(scipen = 999)  # turns of scientific notations like 1e+40
  
  test <- table(mini_htx$subject_sex, mini_htx$district)
  new_df <- data.frame(matrix(ncol = 3, nrow = 0))
  for (row in rownames(test)){
    for (col in colnames(test)){
      #row in new dataframe
      #(sex, district, value)
      new_df <- rbind(new_df, c(row, col, test[row, col]))
    }
  }
  colnames(new_df) <- c("sex", "district", "count")
  new_df$count <- as.numeric(new_df$count)
  new_df$district <- as.factor(new_df$district)
  
  mens <- new_df[new_df$sex == "male",]
  mens$count <- (mens$count * -1)
  new_df <- rbind(new_df[new_df$sex == "female",], mens)
  new_df$district <- ordered(new_df$district, seq(1,24))
  
  brks <- seq(-50000, 30000, 10000)
  lbls = paste0(as.character(c(seq(500, 0, -100), seq(100, 300, 100))), "k")
  #lbls = paste0(as.character(c(seq(15, 0, -5), seq(5, 15, 5))), "m")
  
  # Plot
  
  ggplot(new_df, aes(x = district, y = count, fill = sex)) +   # Fill column
    geom_bar(stat = "identity", width = .7) +   # draw the bars
    scale_y_continuous(breaks = brks, labels = lbls) + 
    coord_flip() +  # Flip axes
    labs(title="Citation by Sex by District - Pyramid", y = "Citation Count", x = "District") +
    theme_tufte() +  # Tufte theme from ggfortify
    theme(plot.title = element_text(hjust = .5), 
          axis.ticks = element_blank()) +   # Centre plot title
    scale_fill_brewer(palette = "Dark2")  # Color palette
  
}