library(shiny)

# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Test Data Certification"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Copy the line below to make a file upload manager
      fileInput("file1", label = h3("Upload File for Environment 1")),
     
      tags$hr(),
      
      # Copy the line below to make a file upload manager
      fileInput("file2", label = h3("Upload File for Environment 2")),
      
      tags$hr(),
      actionButton("update1", "Display Result 1 "),
      tags$hr(),
      actionButton("update2", "Display Result 2 ")
      
    
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      fluidRow(
        # Output: Data file ----
        column(6,
        tableOutput("contents1")),
        column(6,
        tableOutput("contents2"))
      )
    )
  )
)

# Define server logic to read selected file ----
server <- function(input, output) {
  
  output$contents1 <- renderTable({
   
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath)
   df1 = head(df1)
   
  })
  
  output$contents2 = renderTable({
    req(input$file2)
    
    df2 <- read.csv(input$file2$datapath)
    df2 = head(df2)
  })
 
}

# Create Shiny app ----
shinyApp(ui, server)







