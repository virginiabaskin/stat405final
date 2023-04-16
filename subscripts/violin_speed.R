source("shiny_plot_setup.R")

violin_speed <- function(){
  mini_htx4 <- na.omit(mini_htx2[,c("subject_race", "date", "diff", "subject_sex")])
  mini_htx4 <- mini_htx4[mini_htx4$diff != 693,]
  
  ggplot(mini_htx4, aes(fill=factor(subject_sex),subject_race, diff)) + 
    geom_violin() + 
    labs(title="MPH over Speed Limit for Citation", x="Subject Race",y="MPH Speeding")+
    guides(fill=guide_legend(title='Subject Sex'))
}