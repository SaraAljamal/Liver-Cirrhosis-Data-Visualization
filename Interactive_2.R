library(ggplot2)
library(dplyr)
library(plotly)
library(shiny)
library(data.table)
library(gridExtra)
library(corrplot)

df <- read.csv("C:/Users/Sara Aljamal/Desktop/HTU/Third year/Second semester/Data visualization/Assignment/liver_cirrhosis_preprocessed.csv")

mean_n_days_by_stage_status <- df %>%
  group_by(Stage, Status) %>%
  summarise(mean_N_Days = mean(N_Days, na.rm = TRUE), .groups = 'drop')%>%
  mutate(mean_N_Days = as.integer(mean_N_Days)) %>%
  ungroup()
print(mean_n_days_by_stage_status)


ui <- fluidPage(
  titlePanel("Mean n days Across Status and Stage"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("stage", "Select Stage:",
                         choices = unique(mean_n_days_by_stage_status$Stage),
                         selected = unique(mean_n_days_by_stage_status$Stage)[1])
    ),
    mainPanel(
      plotOutput("barplot")
    )
  )
)


server <- function(input, output) {
  
  filtered_data <- reactive({
    subset(mean_n_days_by_stage_status, Stage %in% input$stage)
  })
  
  output$barplot <- renderPlot({
    custom_palette <- c("1" = "#41E685", "2" = "#5688AD", "3" = "#8F56AD")
    ggplot(data = filtered_data(), aes(x = Status, y = mean_N_Days, fill = factor(Stage))) +
      geom_bar(stat = "identity", position = position_dodge()) +
      labs(x = "Status", y = "Mean n days") +
      scale_fill_manual(values = custom_palette) + 
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)

