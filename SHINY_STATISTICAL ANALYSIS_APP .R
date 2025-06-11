library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(tools)

ui <- fluidPage(
  titlePanel("Differential and Inferential Statistical Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File", accept = ".csv"),
      uiOutput("varselect"),
      hr(),
      h4("Inferential Statistics"),
      uiOutput("infer_select_1"),
      uiOutput("infer_select_2"),
      radioButtons("test_type", "Select Test Type", choices = c("t-test", "Chi-squared Test")),
      actionButton("run_test", "Run Hypothesis Test"),
      hr(),
      uiOutput("confint_var"),
      numericInput("conf_level", "Confidence Level", value = 0.95, min = 0.8, max = 0.99, step = 0.01),
      hr(),
      uiOutput("correlation_vars")
    ),
    
    mainPanel(
      h3("Descriptive Statistics"),
      verbatimTextOutput("summaryStats"),
      
      h3("Box Plots"),
      plotOutput("boxPlot1"),
      plotOutput("boxPlot2"),
      
      h3("Hypothesis Test Results"),
      verbatimTextOutput("testResult"),
      
      h3("Confidence Interval"),
      verbatimTextOutput("confint_result"),
      
      h3("Correlation Analysis"),
      verbatimTextOutput("correlation"),
      plotOutput("corPlot")
    )
  )
)

server <- function(input, output, session) {
  # Load data
  dataInput <- reactive({
    req(input$file)
    ext <- file_ext(input$file$name)
    validate(need(ext == "csv", "Please upload a CSV file"))
    read_csv(input$file$datapath)
  })
  
  # UI for selecting variables
  output$varselect <- renderUI({
    req(dataInput())
    df <- dataInput()
    tagList(
      selectInput("num_var", "Select Numerical Variable", choices = names(df), selected = names(df)[1]),
      selectInput("cat_var1", "Select First Categorical Variable", choices = names(df), selected = names(df)[2]),
      selectInput("cat_var2", "Select Second Categorical Variable", choices = names(df), selected = names(df)[3])
    )
  })
  
  # Descriptive Statistics
  output$summaryStats <- renderPrint({
    req(input$num_var, input$cat_var1, input$cat_var2)
    df <- dataInput()
    num_var <- df[[input$num_var]]
    cat1 <- df[[input$cat_var1]]
    cat2 <- df[[input$cat_var2]]
    
    stats <- list(
      Mean = mean(num_var, na.rm = TRUE),
      Median = median(num_var, na.rm = TRUE),
      Mode = as.numeric(names(sort(table(num_var), decreasing = TRUE)[1]))
    )
    print(stats)
  })
  
  # Boxplots
  output$boxPlot1 <- renderPlot({
    req(input$num_var, input$cat_var1)
    ggplot(dataInput(), aes_string(x = input$cat_var1, y = input$num_var)) +
      geom_boxplot(fill = "lightblue") + theme_minimal()
  })
  
  output$boxPlot2 <- renderPlot({
    req(input$num_var, input$cat_var2)
    ggplot(dataInput(), aes_string(x = input$cat_var2, y = input$num_var)) +
      geom_boxplot(fill = "lightgreen") + theme_minimal()
  })
  
  # Inferential Statistics Variable Selectors
  output$infer_select_1 <- renderUI({
    req(dataInput())
    selectInput("var1", "Variable 1", choices = names(dataInput()))
  })
  
  output$infer_select_2 <- renderUI({
    req(dataInput())
    selectInput("var2", "Variable 2", choices = names(dataInput()))
  })
  
  # Run Hypothesis Test
  observeEvent(input$run_test, {
    output$testResult <- renderPrint({
      req(input$var1, input$var2)
      df <- dataInput()
      var1 <- df[[input$var1]]
      var2 <- df[[input$var2]]
      
      if (input$test_type == "t-test") {
        if (is.numeric(var1) && is.factor(var2)) {
          res <- t.test(var1 ~ var2)
          print(res)
        } else {
          print("t-test requires numeric ~ categorical input")
        }
      } else if (input$test_type == "Chi-squared Test") {
        if (is.factor(var1) && is.factor(var2)) {
          tbl <- table(var1, var2)
          res <- chisq.test(tbl)
          print(res)
        } else {
          print("Chi-squared test requires two categorical variables")
        }
      }
    })
  })
  
  # Confidence Intervals
  output$confint_var <- renderUI({
    req(dataInput())
    num_vars <- names(Filter(is.numeric, dataInput()))
    selectInput("conf_var", "Select Variable for Confidence Interval", choices = num_vars)
  })
  
  output$confint_result <- renderPrint({
    req(input$conf_var, input$conf_level)
    df <- dataInput()
    x <- df[[input$conf_var]]
    x <- na.omit(x)
    n <- length(x)
    se <- sd(x)/sqrt(n)
    mean_x <- mean(x)
    alpha <- 1 - input$conf_level
    z <- qnorm(1 - alpha/2)
    ci <- mean_x + c(-1, 1) * z * se
    cat("Confidence Interval (", input$conf_level * 100, "%):\n", sep = "")
    print(ci)
  })
  
  # Correlation
  output$correlation_vars <- renderUI({
    req(dataInput())
    num_vars <- names(Filter(is.numeric, dataInput()))
    tagList(
      selectInput("cor_var1", "Correlation Var 1", choices = num_vars),
      selectInput("cor_var2", "Correlation Var 2", choices = num_vars)
    )
  })
  
  output$correlation <- renderPrint({
    req(input$cor_var1, input$cor_var2)
    df <- dataInput()
    cor_val <- cor(df[[input$cor_var1]], df[[input$cor_var2]], use = "complete.obs")
    cat("Correlation Coefficient between", input$cor_var1, "and", input$cor_var2, "is:\n")
    print(cor_val)
  })
  
  output$corPlot <- renderPlot({
    req(input$cor_var1, input$cor_var2)
    ggplot(dataInput(), aes_string(x = input$cor_var1, y = input$cor_var2)) +
      geom_point(color = "blue") +
      geom_smooth(method = "lm", se = FALSE, col = "red") +
      theme_minimal()
  })
}

shinyApp(ui, server)

