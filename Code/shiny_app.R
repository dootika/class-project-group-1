library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Air Quality and Respiratory Illnesses"),
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Select a dataset:", c("air_quality", "child_2912", "child_78" , "statewise_pollution" , "quality_vs_deaths")),
      selectInput("x_var", "Select X variable:", ""),
      selectInput("y_var", "Select Y variable:", ""),
      selectInput("row_start" , "start row :" , ""),
      selectInput("row_end" , "end row :" , ""),
      # Add input fields for barplot
      selectInput("barplot_x_var", "Select X variable for Barplot:", ""),
      selectInput("barplot_y_var", "Select Y variable for Barplot:", "")
      
    ),
    mainPanel(
      tableOutput("data"),
      actionButton("plot_button", "Generate Scatterplot"),
      actionButton("barplot_button", "Generate Barplot"),
      plotOutput("scatterplot"),
      plotOutput("barplot") 
    )
  )
)

server <- function(input, output) {
  # Sample data for demonstration
  # Replace this with your own datasets
  air_quality <- read.csv("./Data_Air_Quality/air_quality.csv")
  child_2012 <- read.csv("./Data_Respiratory_illnesses/child_2012.csv")
  child_78 <- read.csv("./Data_Respiratory_illnesses/child_78.csv")
  statewise_pollution <- read.csv("./Data_Air_Quality/statewise_pollution.csv")
  quality_vs_deaths <- read.csv("./Data_Air_Quality/quality_vs_deaths.csv")
  
  datasets <- list("air_quality" = air_quality, "child_2012" = child_2012, "child_78" = child_78 , 
                   "statewise_pollution" = statewise_pollution , "quality_vs_deaths" = quality_vs_deaths)
  
  # Show data summary
  output$data <- renderTable({
    if (!is.null(input$dataset)) {
      datasets[[input$dataset]][input$row_start:input$row_end , c(input$x_var , input$y_var)]
    }
  })
  
  # Create scatterplot
  output$scatterplot <- renderPlot({
    if (!is.null(input$dataset) && input$plot_button > 0) {
      data <- datasets[[input$dataset]]
      ggplot(data, aes(x = data[, input$x_var], y = data[, input$y_var] )) +
        geom_point() +
        labs(title = paste("Scatterplot for", input$dataset)) + 
        xlab(input$x_var) +
        ylab(input$y_var)
    }
    
  # Create barplot
output$barplot <- renderPlot({
  if (!is.null(input$dataset) && input$plot_button > 0) {
    data <- datasets[[input$dataset]]
    x_var <- input$barplot_x_var
    y_var <- input$barplot_y_var
    
    # Create the barplot
    ggplot(data, aes(x = .data[[x_var]], y = .data[[y_var]])) +
      geom_bar(stat = "identity", fill = "blue") +
      labs(title = paste("Barplot for", input$dataset)) + 
      xlab(x_var) +
      ylab(y_var)
  }
})
  })
  
  # Update variable choices based on selected dataset
  observe({
    if (!is.null(input$dataset)) {
      updateSelectInput(session = getDefaultReactiveDomain(), "x_var", choices = names(datasets[[input$dataset]]))
      updateSelectInput(session = getDefaultReactiveDomain(), "y_var", choices = names(datasets[[input$dataset]]))
      updateSelectInput(session = getDefaultReactiveDomain() , "row_start" , choices = 1:dim(datasets[[input$dataset]])[1])
      updateSelectInput(session = getDefaultReactiveDomain() , "row_end" , choices = 1:dim(datasets[[input$dataset]])[1])
      
      
      # Update variable choices for barplot
      updateSelectInput(session = getDefaultReactiveDomain(), "barplot_x_var", choices = names(datasets[[input$dataset]]))
      updateSelectInput(session = getDefaultReactiveDomain(), "barplot_y_var", choices = names(datasets[[input$dataset]]))
    
      
      
      }
  })
}

shinyApp(ui, server)
