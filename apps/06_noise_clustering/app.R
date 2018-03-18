#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggvis)

# Define UI for application that draws a histogram
###want dataset with slider
###increase amplitude (range of noise) via multiplier?
###maybe add filtering parameter?

iris1 <- data.frame(ID=rownames(iris),iris)
iris1 <- iris1[sample(nrow(iris1),size = 30),]
#iris2 <- melt(iris1, ID=ID)
irisRow <- nrow(iris1)
irisCol <- 4
noisemat <- matrix(nrow = irisRow, ncol=irisCol, data = rnorm(n=irisRow * irisCol, mean = 0, sd = 1))

  ui <- fluidPage(
    titlePanel("Exploring Clusters in Iris Data"),
    sidebarLayout(
      sidebarPanel(
        sliderInput(inputId = "noise", label="Noise", min=0, max = 4, value = 0, step= 0.2),
        numericInput("clusters", "Cluster Count", 3, min = 1, max=9),
        dataTableOutput("table")
      ),
      mainPanel(
        ggvisOutput("plot2")
      )
    )
  )

  server <- function(input, output) {
    iris3 <- reactive({

      out <- iris1[,2:5] + (noisemat * input$noise)
      out <- data.frame(ID=iris1[,1], out, Species=iris1[,6])
      out
    })

    iris4 <- reactive({
      tidyr::gather(iris3(), -ID)
    })

    clusters <- reactive({
      #select distance metric

      distMat <- as.dist(1-cor(t(iris3()[,-c(1,6)])))

      #run clustering algorithm and cut tree to number of clusters
      clusts <- cutree(hclust(distMat), k = input$clusters)
      clusts
    })

    output$table <- renderDataTable({
      #count the number of flowers in each cluster
      data.frame(table(clusters()))
    }, options=list(searching = FALSE, paging=FALSE))

    clustPlot <- reactive({

      colPal <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
                  "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999")
      #make cluster assignments compatible with melted iris data frame
      clusts <- as.character(rep(clusters(),4))

      #print(head(iris4()))

      #actual plot
      data.frame(iris4(), clusts) %>%
        #map properties
        ggvis(x=~variable, y=~value) %>%
        #group by ID (so each flower is represented by a line)
        group_by(ID) %>%
        #specify that we want a line and color by cluster assignment
        layer_lines(stroke=~clusts) %>%
        #specify the mapping
        scale_ordinal("stroke", domain=as.character(c(1:9)), range=colPal)

    })

    clustPlot %>% bind_shiny("plot2")
  }

# Run the application
shinyApp(ui = ui, server = server)

