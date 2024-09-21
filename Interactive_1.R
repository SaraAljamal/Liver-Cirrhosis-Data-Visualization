library(ggplot2)
library(dplyr)
library(plotly)
library(shiny)
library(data.table)
library(gridExtra)
library(corrplot)

df <- read.csv("C:/Users/Sara Aljamal/Desktop/HTU/Third year/Second semester/Data visualization/Assignment/liver_cirrhosis_preprocessed.csv")

ui <- fluidPage(
  titlePanel("Age distribution across stages"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("stage", "Select Stage:", choices = unique(df$Stage))
      ,sliderInput("bins","Number of bins:",min = 5, max = 40,value = 25)
    ),
    mainPanel(
      plotlyOutput("plotly_plot")
    )
  )
)

server <- function(input, output) {
  filtered_data <- reactive({
    subset(df, Stage %in% input$stage)
  })
  output$plotly_plot <- renderPlotly({
    custom_palette <- c("1" = "#41E685", "2" = "#5688AD", "3" = "#8F56AD")
    
    plot <- ggplot(data = filtered_data(), aes(x = Age, fill = factor(Stage))) +
      geom_histogram(alpha = 0.7, bins = input$bins) +  
      scale_fill_manual(values = custom_palette) +  
      labs(x = "Age (years)", y = "Density") +
      theme_minimal()
    
  })
}

shinyApp(ui = ui, server = server)

