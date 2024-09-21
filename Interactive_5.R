library(ggplot2)
library(dplyr)
library(plotly)
library(shiny)
library(data.table)
library(gridExtra)
library(corrplot)

df <- read.csv("C:/Users/Sara Aljamal/Desktop/HTU/Third year/Second semester/Data visualization/Assignment/liver_cirrhosis_preprocessed.csv")

cols <- c("Ascites", "Hepatomegaly", "Spiders","Edema")

ui <- fluidPage(
  titlePanel("Categorical columns distribution with the status"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("column", "Select Column:", choices = cols),
      checkboxGroupInput("status", "Select Status:", choices = unique(df$Status))
    ),
    mainPanel(
      plotlyOutput("plotly_plot")
    )
  )
)


server <- function(input, output) {
  
  output$plotly_plot <- renderPlotly({
    custom_palette <- c("C" = "#41E685", "CL" = "#5688AD", "D" = "#8F56AD")
    filtered_df <- df[df$Status %in% input$status, ]
    plot <- ggplot(data = filtered_df, aes_string(x = input$column, fill = "factor(Status)")) +
      geom_bar(position = 'dodge') +  
      scale_fill_manual(values = custom_palette) +  
      labs(title = paste(input$column, "Distribution"), x = input$column, y = "Frequency") +
      theme_bw()
    
    ggplotly(plot)
  })
}

shinyApp(ui = ui, server = server)