#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
source("shiny_plot_setup.R")

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Citation Breakdown by District"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
          sliderInput("districts", "District:",
                      min = 1, max = 24,
                      value = 1, step = 1)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("killerplot")
        )
    )
))
