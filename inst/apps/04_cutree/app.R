#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

## TODO: show number of clusters and membership size
  ui <- fluidPage(
    sidebarLayout(
      sidebarPanel(sliderInput(inputId = "height",label = "Tree Height",min = 0, max=9,value = 3,step = .5)),
      mainPanel(plotOutput("plotHeight"))
    )
  )

  server <- function(input, output){

    tree <- hclust(dist(iris[,1:4]))

    output$plotHeight <- renderPlot({
      plot(tree)
      abline(h = input$height, col="blue")
      rect.hclust(tree, h =input$height)
    })
  }



# Run the application
shinyApp(ui = ui, server = server)

