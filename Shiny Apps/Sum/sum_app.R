#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui = fluidPage(
   
   # Application title
   titlePanel("Sum of two numbers"),
   
   # Number input form 
   sidebarLayout(
      sidebarPanel(
         textInput("one" , "First Integer") ,
         textInput("two" , "Second Integer") ,
         actionButton("add" , "Compute Prediction Quantiles")
      ) ,
      
      # Show a plot of the generated distribution
      mainPanel(
        
      DT::dataTableOutput("quants" , width = 500)
      )
   )
)

# Define server logic required to draw a histogram
server = function(input , output , session) {
  
# Observe the add click and perform a reactive expression
  observeEvent( input$add , {
    x = as.numeric(input$one)
    y = as.numeric(input$two)
    
    # Reactive expression
    n = x + y
    output$sum = renderPrint(n)
  })
}

# Run the application 
shinyApp(ui = ui , server = server)

