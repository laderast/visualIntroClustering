#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

#DBSCAN movie
slideInd <- 1

  ui <- fluidPage(
    sidebarLayout(
      sidebarPanel(actionButton(inputId="reset", "Start Over"),
                   actionButton(inputId="advance", "Advance")
      ),
      mainPanel(imageOutput("distPlot")
      )
    )
  )

  server <- function(input,output){
    values <- reactiveValues(i = 1)

    observe({
      input$advance
      isolate({
        if(values$i < 18) {values$i <- values$i + 1}
        else{values$i <- 19}
      })
    })

    observe({
      input$reset
      isolate({values$i <- 1})
    })

    output$distPlot <- renderImage({

      if(values$i < 10){
        slideOut <- paste("0",values$i,sep="")}
      else{slideOut <- as.character(values$i)}
      filename <- normalizePath(file.path('dbscanMovie/',
                                          paste('Rplot', slideOut, '.png', sep='')))
      #print(filename)
      # Return a list containing the filename
      list(src = filename)
    }, deleteFile = FALSE)
  }


# Run the application
shinyApp(ui = ui, server = server)

