library(shiny)
library(ggplot2)
library(shinythemes)
library(shinyjs)
ui <- fluidPage(
  theme = shinytheme("slate"),
  titlePanel("Air Quality and Respiratory Illnesses"),
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Select a dataset:", c("city_wise_air_quality", "statewise_air_quality" , "Statewise_air_quality_and_ARI_factors" ,
                                                    "state_wise_ARI_and_factors","statewise_air_quality_and_cases_deaths","state_wise_cases_and_deaths")),
      checkboxInput("desc" , "View Info" , FALSE),
      selectInput("x_var", "Select X variable:", ""),
      selectInput("y_var" , "Select Y variable:" , ""),
      selectInput("row_start" , "start row :" , ""),
      selectInput("row_end" , "end row :" , ""),
      # Add input fields for barplot
      selectInput("plot_type" , "Select plot type" , c("scatterplot" , "barplot" , "histplot")),
      uiOutput("bins_ui"),
      uiOutput("checkbox_ui")
      
    ),
    mainPanel(
      tableOutput("data"),
      textOutput("info"),
      textOutput("corr"),
      actionButton("plot_button","generate plot"),
      plotOutput("plot")
    )
  )
)

server <- function(input, output) {
  # Sample data for demonstration
  # Replace this with your own datasets
  city_wise_air_quality <- read.csv("./Data_Air_Quality/air_quality.csv")
  statewise_air_quality <- read.csv("./Data_Air_Quality/statewise_pollution.csv")
  Statewise_air_quality_and_ARI_factors <- read.csv("./Data_Air_Quality/quality_vs_ARI.csv")
  statewise_air_quality_and_cases_deaths <- read.csv("./Data_Air_Quality/quality_vs_deaths.csv")
  state_wise_ARI_and_factors <- read.csv("./Data_Respiratory_illnesses/State_wise_ARI_and_factors.csv")
  state_wise_cases_and_deaths <- read.csv("./Data_Respiratory_illnesses/Deaths_2011.csv")
  
  datasets <- list("city_wise_air_quality" = city_wise_air_quality,  
                   "statewise_air_quality" = statewise_air_quality , "Statewise_air_quality_and_ARI_factors" = Statewise_air_quality_and_ARI_factors ,
                   "statewise_air_quality_and_cases_deaths" = statewise_air_quality_and_cases_deaths,
                   "state_wise_ARI_and_factors" = state_wise_ARI_and_factors , "state_wise_cases_and_deaths" = state_wise_cases_and_deaths)
  
  
  # Show data summary
  output$data <- renderTable({
    if (!is.null(input$dataset)) {
      datasets[[input$dataset]][input$row_start:input$row_end , c(input$x_var , input$y_var)]
    }
  })
  
  output$corr <- renderText({
    if(input$plot_type == "scatterplot"){
      data <- datasets[[input$dataset]]
      paste("correlation:" , cor(data[,input$x_var] , data[,input$y_var]))
    }
    else{
      NULL
    }
  })
  
  output$info <- renderText({
    if(input$desc){
      dataset <- input$dataset
      if(dataset == "city_wise_air_quality"){
        "Dataset that contains data on SO2, NO2, PM10 and PM2.5 levels in various cities."
      }
      else if(dataset == "statewise_air_quality"){
        "Statewise averages of SO2, NO2, PM10 and PM2.5 levels"
      }
      else if(dataset == "Statewise_air_quality_and_ARI_factors"){
        "Combined dataset of statewise pollutant levels along with domestic factors that influence ARI along with ARI prevelance levels"
      }
      else if(dataset == "state_wise_ARI_and_factors"){
        "Dataset of ARI prevelance levels in children along with various domestic factors that might influence this"
      }
      else if(dataset == "statewise_air_quality_and_cases_deaths"){
        "Dataset of statewise pollutant levels along with number of cases and deaths, state populations, and various ratios"
      }
      else if(dataset == "state_wise_cases_and_deaths"){
        "Dataset of statewise number of ARI cases and deaths, state populations and relevant ratios"
      }
    }
    else{
      NULL
    }
  })
  
  output$bins_ui <- renderUI({
    if (input$plot_type == "histplot") {
      sliderInput("bins", "Number of Bins:", min = 1, max = 50, value = 10)
    } else {
      NULL
    }
  })
  
  output$checkbox_ui <- renderUI({
    if (input$plot_type == "scatterplot"){
      checkboxInput("bestfit", "Best Fit Line" , FALSE)
    }
    else{
      NULL
    }
  })
  
 
  
  # Create scatterplot
  output$plot <- renderPlot({
    if (!is.null(input$dataset) && input$plot_button > 0) {
      
      data <- datasets[[input$dataset]]
      if(input$bestfit){
        line <- geom_smooth(method = "lm", se = FALSE, color = "blue")
      }
      else{
        line <- NULL
      }
      
      if(input$plot_type == "scatterplot"){
        ggplot(data, aes(x = data[, input$x_var], y = data[, input$y_var] )) +
          geom_point() +
          labs(title = paste("Scatterplot for", input$dataset)) + 
          xlab(input$x_var) + 
          ylab(input$y_var) + line
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
      
      
    }
  })
}

shinyApp(ui, server)




