library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Dice Histogram App"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("num_dice", "Number of Dice:", min = 1, max = 5, value = 1),
      sliderInput("num_throws", "Number of Throws:", min = 10, max = 1000, value = 100),
      actionButton("update_plot", "Update Plot")
    ),
    
    mainPanel(
      plotOutput("dice_plot"),
      br(),
      dataTableOutput("summary_table")
    )
  )
)

server <- function(input, output) {
  
  dice_data <- reactive({
    num_dice <- input$num_dice
    num_throws <- input$num_throws
    
    dice_rolls <- matrix(sample(1:6, num_dice * num_throws, replace = TRUE), ncol = num_dice)
    
    sums <- rowSums(dice_rolls)
    
    data.frame(Sum = sums)
  })
  
  output$dice_plot <- renderPlot({
    ggplot(dice_data(), aes(x = Sum)) +
      geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
      labs(title = "Dice Sum Histogram", x = "Sum of Dice", y = "Frequency")
  })
  
  output$summary_table <- renderDataTable({
    summary_data <- table(dice_data()$Sum)
    data.frame(Sum = as.numeric(names(summary_data)), Frequency = as.numeric(summary_data))
  })
  
}

shinyApp(ui, server)
