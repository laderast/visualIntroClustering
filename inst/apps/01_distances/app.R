#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#this code chunk looks at three profiles and shows the distance between them.
library(shiny)
library(reshape2)
library(ggplot2)


  timeP <- c(0,10,20,30)
  exampleData <- data.frame(time=timeP, profile1 = (3 * (sin(timeP) + 1 + 3)),
                            profile2 = (0.5*sin(timeP)+1),
                            profile3 = c(1.4, 1.2, 1.1, 0.5))
  exMelt <- melt(exampleData, id.vars="time")

  corDist <- function(x) as.dist(1-cor(t(x)))

  #calculate the two different distance matrices
  eucMat <- as.matrix(dist(t(exampleData[,-1])))
  corMat <- as.matrix(corDist(t(exampleData[,-1])))


  ui <- fluidPage(
    sidebarLayout(
      sidebarPanel(radioButtons(inputId="distance",
                                label="Distance Metric",
                                choices=c("euclidean", "correlation"),
                                selected = "euclidean"),
                   tableOutput("table")
      ),
      mainPanel(plotOutput("distPlot")
      )
    )
  )

  server <- function(input,output){
    output$distPlot <- renderPlot(
      ggplot(exMelt, aes(x=time, y=value, group=variable,
                         colour=variable)) + geom_line() + geom_point()
    )
    output$table <- renderTable(
      outMat <- switch(input$distance,
                       "euclidean" = eucMat, "correlation" = corMat)
    )
  }


  shinyApp(ui = ui, server = server)



