library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Univariate Discrete Random Variable Analysis"),
  sidebarLayout(
    sidebarPanel(
      textInput("values", "Enter a number (separated by comma): ", ""),
      textInput("prob", "Enter probability (separated by comma): ", ""),
      actionButton("calculate", "Calculate"),
      
    ),
    mainPanel(
      plotOutput("plot_pdf"),
      plotOutput("plot_cdf"),
      verbatimTextOutput("output")
    )
  )
)

server <- function(input, output) {
  output$plot_pdf <- renderPlot({
    validate_distri(input$values, input$prob)
    values <- as.numeric(strsplit(input$values, ",")[[1]])
    prob <- as.numeric(strsplit(input$prob, ",")[[1]])
    df <- data.frame(values, prob)
    ggplot(df, aes(x = values, y = prob)) + geom_bar(stat = "identity") + 
      labs(title = "Probability Density Function", x = "Values", y = "Probability")
  })
  
  output$plot_cdf <- renderPlot({
    validate_distri(input$values, input$prob)
    values <- as.numeric(strsplit(input$values, ",")[[1]])
    prob <- as.numeric(strsplit(input$prob, ",")[[1]])
    df <- data.frame(values, prob)
    ggplot(df, aes(x = values, y = prob)) + geom_step() + 
      labs(title = "Cumulative Distribution Function", x = "Values", y = "Cumulative Probability")
  })
  
  output$output <- renderPrint({
    validate_distri(input$values, input$prob)
    values <- as.numeric(strsplit(input$values, ",")[[1]])
    prob <- as.numeric(strsplit(input$prob, ",")[[1]])
    average <- sum(values * prob)
    variance <- sum((values - average)^2 * prob)
    print(paste("Mean:", average))
    print(paste("Variance: ", variance))
  })
  
  validate_distri <- function(values, prob) {
    values <- as.numeric(strsplit(values, ",")[[1]])
    prob <- as.numeric(strsplit(prob, ",")[[1]])
    if (length(values) != length(prob)) {
      stop("Number of values and probabilities must match.")
    }
    if(any(prob <0) || any(prob > 1)) {
      stop("Probability must be between the interval [0, 1].")
    }
    if(abs(sum(prob) - 1) > 1e-6) {
      stop("Total Probability must sum to one")
    }
  }
}
shinyApp(ui = ui, server = server)