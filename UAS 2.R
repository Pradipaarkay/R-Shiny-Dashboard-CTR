#CODE YANG DIGUNAKAN
library(shiny)
library(shinydashboard)
library(plotly)
library(ggplot2)
library(gridExtra)
library(shinythemes)
library(DT)
library(psych)
library(scales)


# Initial data
data1 <- data.frame(
  Day = c(1:10),
  Left_Sidebar = c(2.5, 2.7, 2.8, 2.6, 3.0, 2.4, 2.9, 2.5, 2.6, 2.7),
  Center_Page = c(3.8, 3.5, 4.0, 3.7, 3.9, 3.6, 4.1, 3.4, 3.8, 3.9),
  Right_Sidebar = c(3.1, 2.9, 3.0, 3.2, 3.3, 2.8, 3.4, 3.1, 3.2, 3.5)
)

# Additional data
data2 <- data.frame(
  Ad_Placement = rep(c("Left Sidebar", "Center Page", "Right Sidebar"), each = 10),
  CTR = c(2.5, 3.8, 3.1, 2.7, 3.5, 2.9, 2.8, 4.0, 3.0, 2.6, 3.7, 3.2, 3.0, 3.9, 3.3, 2.4, 3.6, 2.8, 2.9, 4.1, 3.4, 2.5, 3.4, 3.1, 2.6, 3.8, 3.2, 2.7, 3.9, 3.5)
)

rv <- reactiveValues(data1 = data1, data2 = data2)
rv

# Define UI
ui <- fluidPage(
  titlePanel("Analisis Efektivitas Strategi Penempatan Produk"),
  sidebarLayout(
    sidebarPanel(
      textInput("day_input", "Day:", ""),
      numericInput("left_sidebar_input", "Left Sidebar RKT:", value = 0),
      numericInput("center_page_input", "Center Page RKT:", value = 0),
      numericInput("right_sidebar_input", "Right Sidebar RKT:", value = 0),
      actionButton("add_data_button", "Add Data", class = "btn-primary")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Data Table",
          DTOutput("data_table")
        ),
        tabPanel(
          "Graphs & Analysis",
          plotlyOutput("scatter_plot"),
          h3("ANOVA Analysis Results"),
          verbatimTextOutput("anova_output"),
          verbatimTextOutput("anova_conclusion")
        )
      )
    )
  )
)

# Define Server
server <- function(input, output) {
  
  # Combine both datasets when button is pressed
  observeEvent(input$add_data_button, {
    day <- as.numeric(input$day_input)
    left_sidebar_rkt <- as.numeric(input$left_sidebar_input)
    center_page_rkt <- as.numeric(input$center_page_input)
    right_sidebar_rkt <- as.numeric(input$right_sidebar_input)
    
    new_data1 <- data.frame(Day = day, Left_Sidebar = left_sidebar_rkt, Center_Page = center_page_rkt, Right_Sidebar = right_sidebar_rkt)
    rv$data1 <- rbind(rv$data1, new_data1)
    
    new_data2 <- data.frame(Ad_Placement = rep(c("Left Sidebar", "Center Page", "Right Sidebar"), each = 1), CTR = c(left_sidebar_rkt, center_page_rkt, right_sidebar_rkt))
    rv$data2 <- rbind(rv$data2, new_data2)
  })
  
  # Display interactive table for data1
  output$data_table <- renderDT({
    datatable(rv$data1, options = list(pageLength = 5))
  })
  
  # Display interactive scatter plot for data1
  output$scatter_plot <- renderPlotly({
    plot_ly(rv$data1, x = ~Day, y = ~Left_Sidebar, type = 'scatter', mode = 'lines+markers', name = 'Left Sidebar') %>%
      add_trace(y = ~Center_Page, name = 'Center Page', marker = list(color = 'purple'), line = list(color = 'purple')) %>%
      add_trace(y = ~Right_Sidebar, name = 'Right Sidebar', marker = list(color = 'red'), line = list(color = 'red')) %>%
      layout(title = 'Scatter Plot of Day vs. RKT for Different Ad Placements', xaxis = list(title = 'Day'), yaxis = list(title = 'RKT'))
  })
  
  # Adding statistical feature for data2
  observe({
    # Make sure there are at least two observations in each group
      anova_result <- aov(CTR ~ Ad_Placement, data = data2)
      
      # Display Anova test results for data2
      output$anova_output <- renderPrint({
        summary(anova_result)
      })
      
      # Store the p-value for later use
      p_value <- summary(anova_result)$`Pr(>F)`[1]
      
      # Update reactiveValues with the p-value for data2
      rv$p_value_data2 <- p_value
  })
}

# Run the app
shinyApp(ui = ui, server = server)
