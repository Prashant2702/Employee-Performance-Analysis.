library(shiny)
library(DT)
library(ggplot2)
library(plotly)
library(highcharter)
library(randomForest)

# Define UI for the homepage
ui <- fluidPage(
  tags$head(
    tags$style(
      HTML(
        "
        body {
          background-image: url('https://www.aeologic.com/blog/wp-content/uploads/2020/01/IT-Industry-in-India.png');
          background-size: cover;
          background-position: center;
          background-repeat: no-repeat;
        }
        "
      )
    )
  ),
  navbarPage(
    "Employee Performance Analysis",
    tabPanel("Home",
             titlePanel("Employee Performance Analysis"),
             sidebarLayout(
               sidebarPanel(
                 fileInput("file", "Upload a CSV file"),
                 actionButton("load_data", "Load Data")
               ),
               mainPanel(
                 textOutput("upload_status"),
                 width = 6,
                 uiOutput("visualization_type"),
                 uiOutput("visualization_output"),
                 verbatimTextOutput("data_summary")  # To display data summary
               )
             )
    ),
    tabPanel("Machine Learning",
             sidebarLayout(
               sidebarPanel(
                 numericInput("age", "Age:", min = 23, max = 53, value = 35),
                 numericInput("gender", "Gender (1=Male, 2=Female):", min = 1, max = 2, value = 1),
                 numericInput("Education.Level", "Education level:", min = 1, max = 3, value = 1),
                 numericInput("Years.of.Experience", "Years of experience:", min = 1, max = 25, value = 1),
                 actionButton("predict_button", "Predict"),
                 br(),
                 h3("Machine Learning Results:"),
                 textOutput("prediction_result")
               ),
               mainPanel(
                 plotOutput("generated_plot")
               )
             )
    ),
    tabPanel("Visualization")
  )
)

# Define server logic for the homepage
server <- function(input, output, session) {
  data <- reactive({
    req(input$load_data)
    if (is.null(input$file)) return(NULL)
    read.csv(input$file$datapath)
  })
  
  cleaned_data <- reactive({
    data <- data()
    data <- na.omit(data)
    return(data)
  })
  
  output$upload_status <- renderText({
    if (!is.null(data()))
      paste("File uploaded successfully:", input$file$name)
    else
      "Upload a CSV file to get started."
  })
  
  output$visualization_type <- renderUI({
    if (!is.null(data())) {
      selectInput("plot_type", "Select Visualization", 
                  choices = c("Data Table", "Bar Graph", "Histogram", "3D Scatter Plot"))
    }
  })
  
  output$visualization_output <- renderUI({
    if (!is.null(data())) {
      if (input$plot_type == "Data Table") {
        DTOutput("employee_table")
      } else if (input$plot_type == "Bar Graph") {
        plotlyOutput("bar_chart")
      } else if (input$plot_type == "Histogram") {
        plotOutput("histogram")
      } else if (input$plot_type == "3D Scatter Plot") {
        plotlyOutput("scatter3d_plot")
      }
    }
  })
  
  output$employee_table <- renderDataTable({
    datatable(cleaned_data(), options = list(pageLength = 10, lengthMenu = c(10, 25, 50, 100)))
  })
  
  output$bar_chart <- renderPlotly({
    ggplotly(ggplot(cleaned_data(), aes(x = cleaned_data()[, 1], y = cleaned_data()[, 2])) + 
               geom_bar(stat = "identity"))
  })
  
  output$histogram <- renderPlot({
    ggplot(cleaned_data(), aes(x = cleaned_data()[, 1])) +
      geom_histogram(fill = "blue", bins = 20) +
      labs(title = "Histogram of Employee Data", x = names(cleaned_data())[1])
  })
  
  output$scatter3d_plot <- renderPlotly({
    plot_ly(cleaned_data(), x = ~cleaned_data()[, 1], y = ~cleaned_data()[, 2], z = ~cleaned_data()[, 3], mode = "markers", type = "scatter3d")
  })
  
  output$data_summary <- renderPrint({
    if (!is.null(data())) {
      summary_data <- summary(cleaned_data())
      summary_data
    }
  })
  
  # Machine Learning Server Logic
  observeEvent(input$predict_button, {
    new_data <- data.frame(
      Age = input$age,
      Gender = input$gender,
      Education.Level = input$Education.Level,
      Years.of.Experience = input$Years.of.Experience
    )
    
    # Perform prediction with your machine learning model here
    # Replace the following line with your model's prediction logic
    prediction <- predict(rf_model, new_data)
    
    output$prediction_result <- renderText({
      paste("Predicted Result:", round(prediction, 2))
    })
  })
}

shinyApp(ui, server)