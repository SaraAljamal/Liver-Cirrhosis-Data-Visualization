library(ggplot2)
library(dplyr)
library(plotly)
library(shiny)
library(data.table)
library(gridExtra)
library(corrplot)

df <- read.csv("C:/Users/Sara Aljamal/Desktop/HTU/Third year/Second semester/Data visualization/Assignment/liver_cirrhosis_preprocessed.csv")

cols <- c("Bilirubin", "Cholesterol", "Albumin","Copper","Alk_Phos","SGOT","Tryglicerides","Platelets","Prothrombin")

ui <- fluidPage(
  titlePanel("Numeric columns distribution with the stage"),
  sidebarLayout(
    sidebarPanel(
      selectInput("column", "Select Column:", choices = cols),
      checkboxGroupInput("stage", "Select Stage:", choices = unique(df$Stage))
    ),
    mainPanel(
      plotlyOutput("plotly_plot")
    )
  )
)


server <- function(input, output) {

  output$plotly_plot <- renderPlotly({
    custom_palette <- c("1" = "#41E685", "2" = "#5688AD", "3" = "#8F56AD")
    filtered_df <- df[df$Stage %in% input$stage, ]
    plot <- ggplot(data = filtered_df, aes_string(x = input$column, fill = "factor(Stage)")) +
      geom_density(alpha = 0.5) +  
      scale_fill_manual(values = custom_palette) +  
      labs(title = paste(input$column, "Distribution"), x = input$column, y = "Density") +
      theme_minimal()

    ggplotly(plot)
  })
}

shinyApp(ui = ui, server = server)