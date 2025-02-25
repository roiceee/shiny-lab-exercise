library(shiny)
library(DT)
library(ggplot2)
library(dplyr)

# Load the dataset
data <- read.csv("businessdata.csv")

ui <- fluidPage(
  titlePanel("Business Insights"),
  sidebarLayout(
    sidebarPanel(
      selectInput("page", "Select Page:", choices = c( "Analytics", "Dataset")),
      conditionalPanel(
        condition = "input.page == 'Analytics'",
        selectInput("region", "Select Region:", 
                    choices = c("Overall", unique(data$Customer_Region)), 
                    selected = "Overall")
      )
    ),
    mainPanel(
      conditionalPanel(
        condition = "input.page == 'Analytics'",
        plotOutput("best_sellers_plot")
      ),
      conditionalPanel(
        condition = "input.page == 'Dataset'",
        DTOutput("table")
      )
    )
  )
)

server <- function(input, output) {
  
  output$table <- renderDT({
    datatable(data)
  })
  
  output$best_sellers_plot <- renderPlot({
    filtered_data <- if (input$region == "Overall") {
      data
    } else {
      data %>% filter(Customer_Region == input$region)
    }
    
    best_sellers <- filtered_data %>% 
      group_by(Product) %>% 
      summarise(Total_Sales = sum(Total_Cost, na.rm = TRUE)) %>% 
      arrange(desc(Total_Sales)) %>% 
      head(10)
    
    ggplot(best_sellers, aes(x = reorder(Product, Total_Sales), y = Total_Sales)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      labs(title = "Top 10 Best Selling Products", x = "Product", y = "Total Sales") +
      theme_minimal()
  })
}

# Run App
shinyApp(ui = ui, server = server)
