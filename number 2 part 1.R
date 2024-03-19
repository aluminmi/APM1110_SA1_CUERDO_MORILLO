library(shiny)
library(ggplot2)
ui <- fluidPage(
  titlePanel("Bivariate Discrete Random Variable Analysis"),
  sidebarLayout(
    sidebarPanel(
      textInput("value_x", "Enter x values (separated by comma): ", ""),
      textInput("value_y", "Enter y values (separated by comma): ", ""),
      textInput("prob", "Enter probability (separated by comma): ", ""),
      textInput("cond_x", "Enter x conditional value: ", ""),
      textInput("cond_y", "Enter y conditional value: ", ""),
      actionButton("calculate", "Calculate")
      ),
    mainPanel(
      plotOutput("marginal_plot"),
      plotOutput("condx_plot"),
      plotOutput("condy_plot"),
      hr(),
      h4("Marginal Distribution"),
      verbatimTextOutput("margin_output"),
      h4("Conditional Distribution given X"),
      verbatimTextOutput("condx_output"),
      h4("Conditional Distribution given Y"),
      verbatimTextOutput("condy_output")
    )
  )
)

server <- function(input, output) {
  observeEvent(input$calculate, {
    x <- as.numeric(unlist(strsplit(input$value_x, ",")))
    y <- as.numeric(unlist(strsplit(input$value_y, ",")))
    p <- as.numeric(unlist(strsplit(input$prob, ",")))
    
    if (length(x) != length(y) || length(x) != length(p)) {
      return("Error: Lengths of x, y, and probabilities must be the same.")
    }
    
    df <- data.frame(x = rep(x, each = length(y)),
                     y = rep(y, times = length(x)),
                     prob = p)
    
    marginal <- aggregate(prob ~ x, data = df, sum)
    
    if (!is.null(input$cond_x) && input$cond_x != "" && as.numeric(input$cond_x) %in% x) {
      condx_output <- aggregate(prob ~ y, data = df[df$x == as.numeric(input$cond_x), ], sum)
    } else {
      condx_output <- NULL
    }
    
    if (!is.null(input$cond_y) && input$cond_y != "" && as.numeric(input$cond_y) %in% y) {
      condy_output <- aggregate(prob ~ x, data = df[df$y == as.numeric(input$cond_y), ], sum)
    } else {
      condy_output <- NULL
    }
    
    output$margin_output <- renderPrint(marginal)
    output$condx_output <- renderPrint({
      if (!is.null(condx_output)) condx_output else "No valid conditional value selected for X."
    })
    output$condy_output <- renderPrint({
      if (!is.null(condy_output)) condy_output else "No valid conditional value selected for Y."
    })
    
    output$marginal_plot <- renderPlot({
      ggplot(marginal, aes(x = factor(x), y = prob)) + 
        geom_bar(stat = "identity") +
        labs(x = "X", y = "Probability", title = "Marginal Distribution")
    })
    
    output$condx_plot <- renderPlot({
      if (!is.null(condx_output)) {
        ggplot(condx_output, aes(x = factor(y), y = prob)) + 
          geom_bar(stat = "identity") +
          labs(x = "Y", y = "Probability", title = paste("Conditional Distribution (X =", input$cond_x, ")"))
      } else {
        NULL
      }
    })
    
    output$condy_plot <- renderPlot({
      if (!is.null(condy_output)) {
        ggplot(condy_output, aes(x = factor(x), y = prob)) + 
          geom_bar(stat = "identity") +
          labs(x = "X", y = "Probability", title = paste("Conditional Distribution (Y =", input$cond_y, ")"))
      } else {
        NULL
      }
    })
  })
}


shinyApp(ui = ui, server = server)
