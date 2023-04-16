

source("shiny_plot_setup.R")

#Race waffle plot based on citation counts  
df <- data.frame(table(na.omit(mini_htx$subject_race)))
total_freq <- sum(df$Freq)
df$prop <- df$Freq/total_freq
num_squares <- round(df$prop*100)

waffle <- function(){  
  df_grid <- data.frame(
    x = rep(1:10, each = 10),
    y = rep(1:10, times = 10)
  )
  num_squares <- c(3, 36, 1, 3, 57) #manually add one more square because the rounding resulted in only 99 
  #num_squares <- round(df$prop*100)
  
  df_grid$category <- rep(df$Var1, num_squares)
  ggplot(df_grid, aes(x = x, y = y, fill = category)) +
    geom_tile(color = "black", size = 0.5) +
    scale_fill_brewer(palette = "Set3") +
    labs(title="Waffle Chart of Citations per Race")+
    theme_void()
}

