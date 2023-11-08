library(shiny)
library(ggplot2)
library(shinythemes)
library(shinyjs)
ui <- fluidPage(
  theme = shinytheme("slate"),
  titlePanel("Air Quality and Respiratory Illnesses"),
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Select a dataset:", c("air_quality", "child_2012", "child_78" , "statewise_pollution" , "quality_vs_ARI")),
      selectInput("x_var", "Select X variable:", ""),
      uiOutput("y_var_ui"),
      selectInput("row_start" , "start row :" , ""),
      selectInput("row_end" , "end row :" , ""),
      # Add input fields for barplot
      selectInput("plot_type" , "Select plot type" , c("scatterplot" , "barplot" , "histplot")),
      uiOutput("bins_ui")
      
    ),
    mainPanel(
      tableOutput("data"),
      actionButton("plot_button","generate plot"),
      plotOutput("plot")
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
  quality_vs_ARI <- read.csv("./Data_Air_Quality/quality_vs_ARI.csv")
  
  
  datasets <- list("air_quality" = air_quality, "child_2012" = child_2012, "child_78" = child_78 , 
                   "statewise_pollution" = statewise_pollution , "quality_vs_ARI" = quality_vs_ARI)
  
  
  # Show data summary
  output$data <- renderTable({
    if (!is.null(input$dataset)) {
      datasets[[input$dataset]][input$row_start:input$row_end , c(input$x_var , input$y_var)]
    }
  })
  
  output$bins_ui <- renderUI({
    if (input$plot_type == "histplot") {
      sliderInput("bins", "Number of Bins:", min = 1, max = 50, value = 10)
    } else {
      NULL
    }
  })
  
  output$y_var_ui <- renderUI({
    if (input$plot_type != "histplot") {
      selectInput("y_var", "Select Y Variable:", "")
    } else {
      NULL
    }
    
  })
  
  # Create scatterplot
  output$plot <- renderPlot({
    if (!is.null(input$dataset) && input$plot_button > 0) {
      
      data <- datasets[[input$dataset]]
      
      if(input$plot_type == "scatterplot"){
        ggplot(data, aes(x = data[, input$x_var], y = data[, input$y_var] )) +
          geom_point() +
          labs(title = paste("Scatterplot for", input$dataset)) + 
          xlab(input$x_var) +
          ylab(input$y_var)
      }
      
      else if(input$plot_type == "barplot"){
        
        data <- datasets[[input$dataset]]
        x_var <- input$barplot_x_var
        y_var <- input$barplot_y_var
        
        # Create the barplot
        ggplot(data, aes(x = data[, input$x_var], y = data[, input$y_var])) +
          geom_bar(stat = "identity", fill = "blue") +
          labs(title = paste("Barplot for", input$dataset)) + 
          xlab(x_var) +
          ylab(y_var)
        
      }
      
      else if (input$plot_type == "histplot") {
        x_var <- input$x_var
        bins <- input$bins # You can adjust the number of bins as needed
        
        ggplot(data, aes_string(x = x_var)) +
          geom_histogram(bins = bins, fill = "blue", color = "black") +
          labs(title = paste("Histogram for", input$dataset)) +
          xlab(x_var) +
          ylab("Frequency")
      }
    }
    
  })
  
  
  
  # Update variable choices based on selected dataset
  observe({
    if (!is.null(input$dataset)) {
      updateSelectInput(session = getDefaultReactiveDomain(), "x_var", choices = names(datasets[[input$dataset]]))
      updateSelectInput(session = getDefaultReactiveDomain(), "y_var", choices = names(datasets[[input$dataset]]))
      updateSelectInput(session = getDefaultReactiveDomain() , "row_start" , choices = 1:dim(datasets[[input$dataset]])[1])
      updateSelectInput(session = getDefaultReactiveDomain() , "row_end" , choices = 1:dim(datasets[[input$dataset]])[1])
      
      if (input$plot_type == "histplot") {
        # If the plot type is "histplot," hide the y_var input
        shinyjs::hide("y_var")
      } else {
        # If the plot type is not "histplot," enable the y_var input
        shinyjs::show("y_var")
      }
    }
  })
}

shinyApp(ui, server)




