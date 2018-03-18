#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

#dbscan()
#need reachability parameter
library(fpc)

##This code is modified from Joe cheng's k-means example.
##modify number of iterations
##specify cluster centers - start point

## TODO: add cluster summary table

  ui <- fluidPage(
    sidebarLayout(
      sidebarPanel( radioButtons(inputId="algorithm",
                                 label = "Algorithm",
                                 choices=c("kmeans", "dbscan"),
                                 selected="kmeans"),
                    uiOutput("ui")

      ),
      mainPanel(plotOutput("plotClust"))
    )
  )

  server <- function(input, output){
    newIris <- iris[,c(1:2)]

    #start with the same random centers
    centers <- as.matrix(newIris[c(2,50,100),])

    output$ui <- renderUI({

      # Depending on input$input_type, we'll generate a different
      # UI component and send it to the client.
      switch(input$algorithm,
             "kmeans" = numericInput(inputId = "clusters",
                                     label = "Number of Clusters (k)",
                                     min = 1, max=10,value = 2),
             "dbscan" = sliderInput(inputId = "eps", "Reachability Distance",
                                    min = 0.001, max = 0.5,
                                    value = 0.1)
      )
    })

    clusters <- reactive({
      if(input$algorithm == "kmeans"){
        out <- suppressWarnings(kmeans(newIris, centers = input$clusters))}
      if(input$algorithm == "dbscan"){
        out <- dbscan(newIris, eps=input$eps)

        #change unreachable points to have value 10 (so they'll be plotted
        #as light grey)
        out$cluster <- ifelse(out$cluster == 0,10,out$cluster)
        out$seeds <- newIris[which(out$isseed),]
      }
      out
    })

    output$plotClust <- renderPlot({
      par(mar = c(5.1, 4.1, 0, 1))
      palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
                "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#994C00", "#C0C0C0"))
      plot(newIris, col = clusters()$cluster, pch = 20, cex = 3)
      if(input$algorithm == "kmeans"){
        points(clusters()$centers, pch = 4, cex = 4, lwd = 4)}
      if(input$algorithm == "dbscan"){
        #points(clusters()$seeds, pch = 6, cex = 4, lwd = 4)
      }
    })
  }




# Run the application
shinyApp(ui = ui, server = server)

