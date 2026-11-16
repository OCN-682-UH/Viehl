#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)



# shiny code!
ui <- fluidPage(
  sliderInput(inputId = "num",
              label = "Choose a number",
              value = 50, min = 1, max = 100
              ),
  textInput(inputId = "title",
            label = "Write a title",
            value = "Histogram of Random Normal Values"
            ),
  plotOutput("hist"),
  verbatimTextOutput("stats")
  
)


server <- function(input, output){
  output$hist <- renderPlot({
    
    
    data <- reactive({
      tibble(x = rnorm(input$num))
    })
    
    
    ggplot(data(), aes(x = x)) +
      
      geom_histogram() +
      labs(title = input$title)
    
  })
  
  output$stats <- renderPrint({
    summary(data())
  })
  
}

shinyApp(ui = ui, server = server)



