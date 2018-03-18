#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
#TODO: line plot where points are colored by scaling

library(gplots)
library(cluster.datasets)
library(plotly)
data("european.foods")
ef <- european.foods[,-c(1,2)]
rownames(ef) <- make.names(european.foods$name)
colnames(ef) <- c("WestGermany", "Italy", "France", "Netherlands", "Belgium",
                  "Luxembourg", "Great Britain", "Portugal", "Austria", "Switzerland",
                  "Sweden", "Denmark", "Norway", "Finland", "Spain", "Ireland")

ef[2,] <- ef[2,]/2
ef[3,] <- ef[2,]/5

food_names <- rownames(ef)

ef2 <- data.frame(foods=rownames(ef),ef)

scale_this <- function(x){ as.vector(scale(x))}

library(cowplot)

ef_long <- ef2 %>% tidyr::gather(key = "country", value="percent", -foods) %>%
  group_by(foods) %>% mutate(zscore=scale_this(percent)) %>% ungroup() %>% group_by(country) %>%
  mutate(zscore2=scale_this(percent)) %>% ungroup()


#plot_grid(plot1, plot2, nrow=2)
methods = c("none"="percent", "row"="zscore", "column"="zscore2")

ui <- fluidPage(sidebarLayout(
  sidebarPanel(selectInput("num_rows", "select number of rows to show", choices= 1:3, selected=1),
               selectInput("scaling", "select scaling method", choices=methods, selected=methods[1])),
  mainPanel(plotlyOutput("clust_plot1"), plotlyOutput("clust_plot2"))
))

server <- function(input, output){

  ef_long_data <- reactive({
    subset <- food_names[1:input$num_rows]
    ef_long %>% filter(foods %in% subset)

  })

  output$clust_plot1 <- renderPlotly({
    out_graph <- ef_long_data() %>% #dplyr::filter(foods %in% c("ground.coffee", "instant.coffee")) %>%
      ggplot(aes_string(x="country", y="percent", group="foods")) + geom_line() + geom_point(aes_string(color=input$scaling), size=5, shape=15) +
      scale_color_distiller(type="div") + theme(axis.text.x = element_blank(), axis.text.y = element_blank())
    out_graph %>% ggplotly()
  })

  output$clust_plot2 <- renderPlotly({
    out_graph2 <- ef_long_data() %>% #dplyr::filter(foods %in% c("ground.coffee", "instant.coffee")) %>%
      ggplot(aes_string(x="country", y="foods", fill=input$scaling)) + geom_tile() + scale_fill_distiller(type="div") +
      theme(axis.text.y = element_blank(), axis.text.x = element_text(angle=90))

    out_graph2 %>% ggplotly()
  })
}

# Run the application
shinyApp(ui = ui, server = server)

