library(shiny)
library(ggplot2)
library(rsconnect)

ui <- fluidPage(
  titlePanel("Discrete Random Variable Analysis"),
  tabsetPanel(
    tabPanel("Bivariate Analysis",
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
    ),
    tabPanel("Univariate Analysis",
             sidebarLayout(
               sidebarPanel(
                 textInput("values", "Enter a number (separated by comma): ", ""),
                 textInput("prob_uni", "Enter probability (separated by comma): ", ""),
                 actionButton("calculate_uni", "Calculate")
               ),
               mainPanel(
                 plotOutput("plot_pdf"),
                 plotOutput("plot_cdf"),
                 verbatimTextOutput("output")
               )
             )
    ),
    tabPanel("Defective Rates",
    sidebarLayout(
      sidebarPanel(
        textInput("x1", "Factory 1 (Proportion)", ""),
        textInput("x2", "Factory 2 (Proportion)", ""),
        textInput("x3", "Factory 3 (Proportion)"),
        textInput("y1", "Factory 1 (Defective Rate)", ""),
        textInput("y2", "Factory 2 (Defective Rate)", ""),
        textInput("y3", "Factory 3 (Defective Rate)", ""),
        actionButton("calculate_custom", "Calculate")
      ),
      mainPanel(
        verbatimTextOutput("custom_output")
      )
    )
  ),
  tabPanel("Simulation Analysis", sidebarLayout(
    sidebarPanel(
      numericInput("p_sim", "Enter the probability (in %):", value = 50, min = 0, max = 100),
      actionButton("simulate_search_button", "Simulate")
    ),
    mainPanel(
      plotOutput("simulated_hist"),
      verbatimTextOutput("simulated_output")
      )
    ))
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
  
  observeEvent(input$calculate_uni, {
    validate_distri(input$values, input$prob_uni)
    values <- as.numeric(strsplit(input$values, ",")[[1]])
    prob <- as.numeric(strsplit(input$prob_uni, ",")[[1]])
    df <- data.frame(values, prob)
    
    output$plot_pdf <- renderPlot({
      ggplot(df, aes(x = values, y = prob)) + geom_bar(stat = "identity") + 
        labs(title = "Probability Density Function", x = "Values", y = "Probability")
    })
    
    output$plot_cdf <- renderPlot({
      ggplot(df, aes(x = values, y = prob)) + geom_step() + 
        labs(title = "Cumulative Distribution Function", x = "Values", y = "Cumulative Probability")
    })
    
    output$output <- renderPrint({
      average <- sum(values * prob)
      variance <- sum((values - average)^2 * prob)
      print(paste("Mean:", average))
      print(paste("Variance: ", variance))
    })
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
  custom_analysis <- function() {
    x1 <- as.numeric(input$x1)
    x2 <- as.numeric(input$x2)
    x3 <- as.numeric(input$x3)
    y1 <- as.numeric(input$y1)
    y2 <- as.numeric(input$y2)
    y3 <- as.numeric(input$y3)
    
    if (!all(c(0.10 <= x1, x1 <= 0.40, 0.10 <= x2, x2 <= 0.40, 0.10 <= x3, x3 <= 0.40))) {
      return("Invalid input for proportions. Proportions must be between 0.10 and 0.40.")
    }
    
    if (abs(x1 + x2 + x3 - 1) > 1e-6) {
      return("Invalid input for proportions. Proportions must sum to 1.")
    }
    
    if (!all(c(0.01 <= y1, y1 <= 0.05, 0.01 <= y2, y2 <= 0.05, 0.01 <= y3, y3 <= 0.05))) {
      return("Invalid input for defective rates. Defective rates must be between 0.01 and 0.05.")
    }
    
    if (abs(y1 + y2 + y3 - 0.12) > 1e-6) {
      return("Invalid input for defective rates. Defective rates must sum to 0.12.")
    }
    
    prob_defect <- x1 * y1 + x2 * y2 + x3 * y3
    return(paste("Probability of selecting a defective product: ", prob_defect))
  }
  
  observeEvent(input$calculate_custom, {
    output$custom_output <- renderPrint({
      custom_analysis()
    })
  })
  
  observeEvent(input$simulate_button, {
    p <- input$p_slider / 100
    search_num <- input$search_num
    
    search_sim <- simulate_search(p, search_num)
    
    output$hist_plot <- renderPlot({
      hist(search_sim, breaks = max(search_sim),
           freq = FALSE, main = "Simulated PDF of Searches",
           xlab = "No. of Searches", 
           ylab = "Probability Density",
           col = "violet")
    })
    output$summary_output <- renderPrint({
      stat_complete <- mean_var(search_sim)
      cat("\nMean of Complete set: ", stat_complete$mean, "\n")
      cat("\nVariance of Complete set: ", stat_complete$variance, "\n")
      
      condi_search <- search_sim[search_sim > 3]
      
      condi_stats <- mean_var(condi_search)
      cat("\nMean of Conditioned set: ", condi_stats$mean, "\n")
      cat("\nVariance of Conditioned set: ", condi_stats$variance, "\n")
      
      p4_3 <-sum(search_sim == 4) / sum(search_sim > 3)
      p1 <- sum(search_sim == 1) / length(search_sim)
      
      cat("\nP(X = 4 | X > 3: ", p4_3, "\n")
      cat("\nP(X = 1)", p1, "\n")
      
      p5_3 <-sum(search_sim == 5) / sum(search_sim > 3)
      p2 <- sum(search_sim == 2) / length(search_sim)
      
      cat("\nP(X = 5 | X > 3: ", p5_3, "\n")
      cat("\nP(X = 2)", p2, "\n")
    })
  })
  observeEvent(input$simulate_search_button, {
    p <- input$p_sim / 100
    search_num <- 10000  # Number of simulations
    
    # Simulation
    search_sim <- simulate_search(p, search_num)
    
    # Histogram plot
    output$simulated_hist <- renderPlot({
      hist(search_sim, breaks = max(search_sim),
           freq = FALSE, main = "Simulated PDF of Searches",
           xlab = "No. of Searches", 
           ylab = "Probability Density",
           col = "violet")
    })
    
    # Summary statistics
    output$simulated_output <- renderPrint({
      stat_complete <- mean_var(search_sim)
      cat("\nMean of Complete set: ", stat_complete$mean, "\n")
      cat("\nVariance of Complete set: ", stat_complete$variance, "\n")
      
      condi_search <- search_sim[search_sim > 3]
      condi_stats <- mean_var(condi_search)
      cat("\nMean of Conditioned set: ", condi_stats$mean, "\n")
      cat("\nVariance of Conditioned set: ", condi_stats$variance, "\n")
      
      p4_3 <- sum(search_sim == 4) / sum(search_sim > 3)
      p1 <- sum(search_sim == 1) / length(search_sim)
      cat("\nP(X = 4 | X > 3: ", p4_3, "\n")
      cat("\nP(X = 1)", p1, "\n")
      
      p5_3 <- sum(search_sim == 5) / sum(search_sim > 3)
      p2 <- sum(search_sim == 2) / length(search_sim)
      cat("\nP(X = 5 | X > 3: ", p5_3, "\n")
      cat("\nP(X = 2)", p2, "\n")
    })
  })
}

shinyApp(ui = ui, server = server)
