library(ggplot2)
library(dplyr)
library(plotly)
library(shiny)
library(data.table)
library(gridExtra)
library(corrplot)

df <- read.csv("C:/Users/Sara Aljamal/Desktop/HTU/Third year/Second semester/Data visualization/Assignment/liver_cirrhosis_preprocessed.csv")

cols <- c("Bilirubin", "Cholesterol", "Albumin","Copper","Alk_Phos","SGOT","Tryglicerides","Platelets","Prothrombin")
normal_ranges <- list(
  Bilirubin = c(0.1, 1.2),
  Cholesterol = c(0, 200),
  Albumin = c(3.5, 5),
  Copper = c(70, 140),
  Alk_Phos = c(45, 150),
  SGOT = c(10, 40),
  Tryglicerides = c(0, 150),
  Platelets = c(150, 450),
  Prothrombin = c(11, 13.5)
)

ui <- fluidPage(
  titlePanel("Numeric columns with normal ranges"),
  sidebarLayout(
    sidebarPanel(
      selectInput("column", "Select Column:", choices = cols)
    ),
    mainPanel(
      plotlyOutput("plotly_plot")
    )
  )
)

server <- function(input, output) {
  
  output$plotly_plot <- renderPlotly({
    plot <- ggplot(data = df, aes_string(x = input$column)) +
      geom_histogram(alpha = 0.5, bins=20) +  
      labs(title = paste(input$column, "Distribution with the normal range"), x = input$column, y = "Frequency") +
      geom_vline(xintercept = normal_ranges[[input$column]], color = "#41E685", linetype = "dashed")+
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)