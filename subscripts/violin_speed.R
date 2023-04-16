source("shiny_plot_setup.R")


#mini_htx2$district <- factor(mini_htx$district)
#mini_htx2$diff <- abs(mini_htx2$speed - mini_htx2$posted_speed)

violin_speed <- function(){
  mini_htx4 <- na.omit(mini_htx[,c("subject_race", "date", "diff", "subject_sex")])
  mini_htx4 <- mini_htx4[mini_htx4$diff != 693,]
  mini_htx4 <- mini_htx4[mini_htx4$diff <= 105,]
  
  ggplot(mini_htx4, aes(fill=factor(subject_sex),subject_race, diff)) + 
    geom_violin() + 
    labs(title="MPH over Speed Limit for Citation", x="Subject Race",y="MPH Speeding")+
    guides(fill=guide_legend(title='Subject Sex'))
}