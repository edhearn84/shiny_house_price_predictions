#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

fields = c("name" , "age" , "height" , "weight")

ui = fluidPage(
  
  # Application title
  titlePanel("Health Card") ,
  
  # Sidebar with reactive inputs
  sidebarLayout(
    sidebarPanel(
      textInput("name" , "Your Name") ,
      selectInput("age" , "Age bracket" , c("18-25","25-45","above 45")) ,
      textInput("weight" , "Please enter your weight in kg") ,
      textInput("height" , "Please enter your height in cm") ,
      actionButton("save" , "Add")
      
      
    ) ,
    
  # A table of reactive outputs
  mainPanel(
    mainPanel(
      
      DT::dataTableOutput("responses" , width = 500), tags$hr() 
      
      )
    )
  )
)

# Define server logic 
server = function(input, output,session) {
  
  #create a data frame called responses
  saveData = function(data) {
    data = as.data.frame(t(data))
    if (exists("responses")) {
      responses <<- rbind(responses, data)
    } else {
      responses <<- data
    }
  }
  
  loadData = function() {
    if (exists("responses")) {
      responses
    }
  }
  
  
  # Whenever a field is filled, aggregate all form data
  #formData is a reactive function
  formData = reactive({
    data = sapply(fields, function(x) input[[x]])
    data
  })
  
  # When the Save button is clicked, save the form data
  observeEvent(input$save, {
    saveData(formData())
  })
  
  # Show the previous responses
  # (update with current response when save is clicked)
  output$responses = DT::renderDataTable({
    input$save
    loadData()
  })     
}

# Run the application 
shinyApp(ui = ui , server = server)

