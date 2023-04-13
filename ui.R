#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Citation Breakdown by District"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            checkboxGroupInput(inputId = "districts",
                        "Districts to show:",
                        c("1", "2", "3", "4", "5",
                          "6", "7", "8", "9", "10",
                          "11", "12", "13", "14", "15",
                          "16", "17", "18", "19", "20",
                          "21", "22", "23", "24"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("killerplot")
        )
    )
))
