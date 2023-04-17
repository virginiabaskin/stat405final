#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# import packages



# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$killerplot <- renderPlot({
      grid.newpage()
      
      #legend viewport 
      pushViewport(viewport(
        x = 0.84, y = 0.33, just = c("left", "bottom"),
        width = 0.16, height = 0.57
      ))
      
      #plot legend
      citations <- c("Red/Stop", "Seat Belt", "Financial", "License", "Speeding")
      colors <- c("lightcyan","lightpink","moccasin","skyblue1","thistle1")
      legend(citations, colors)
      
      popViewport()
      
      #main viewport
      pushViewport(viewport(
        x = 0.1, y = 0.1, just = c("left", "bottom"),
        width = 0.8, height = 0.9
      ))
      #axis
      grid.lines(c(1.75/8, 0.9), c(1.5/8, 1.5/8))
      grid.lines(c(1.75/8, 1.75/8), c(1.5/8, 0.9))
      #grid.lines(c(0.1, 0.9), c(0.5, 0.5))
      grid.text("Citation Breakdown by Sex, Race, & Year", #Citation Breakdown by Sex for each Race & Year 
                x = 1.75/8, y = 0.925, just = c("left", "bottom"),
                gp = gpar(fontsize = 18)
      )
      pushViewport(viewport(
        x = 0.1, y = 0.1, just = c("left", "bottom"),
        width = 0.8, height = 0.8
      ))
      #grid.rect(x=0.5, y=0.5, width = 1, height=1)
      rows <- 8
      cols <- 6
      ly <- grid.layout(rows, cols)
      pushViewport(viewport(layout = ly))
      #makes a viewpoint for each 
      
      
      # set row labels
      for (i in 1:(rows-1)) {
        pushViewport(viewport(layout.pos.row=i, layout.pos.col = 1))
        grid.text(years[i])
        popViewport()}
      
      # set column labels
      for (j in 2:cols){
        pushViewport(viewport(layout.pos.row=8, layout.pos.col = j))
        if (races[j-1]=="asian/pacific islander"){
          grid.text("AAPI", gp = gpar(fontsize = 12))
        }
        else{
          grid.text(races[j-1], gp = gpar(fontsize = 12))
        }
        popViewport()
      }
      
      
      for (i in 1:(rows-1)) {
        for (j in 2:cols) {
          
          # push main grid Viewport
          pushViewport(viewport(layout.pos.row=i, layout.pos.col = j))
          
          # push small viewport
          pushViewport(viewport(width=0.75, height=0.75, just="centre"))
          
          # make bar plot
          bar_run(races[j-1], years[i], as.numeric(input$districts))
          
          if (j==2){
            if (i==1){
              grid.text("M", x = 0.1, y = 0.9, gp=gpar(fontsize=8))
              grid.text("F", x = 0.1, y = 0.1, gp=gpar(fontsize=8))
            }
          }
          
          # make circle
          count <- pull(citations_htx_circle[citations_htx_circle['subject_race']==races[j-1] &
                                               citations_htx_circle['date']==years[i] &
                                               citations_htx_circle['district'] == as.numeric(input$districts),
                                             "n_individuals"])[1]
          #max_count <- sum(citations_htx_circle$n_individuals)
          max_count <- max(na.omit(citations_htx_circle[citations_htx_circle["district"]==as.numeric(input$districts),
                                                "n_individuals"]))
          draw_circle(count, max_count)
          
          # pop small viewport
          popViewport()
          
          # pop main grid viewport
          popViewport()
        }
      }
    }, width=600, height=450)

})
