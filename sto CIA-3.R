library(shiny)

# Exponential random variables
simulate_S <- function(t, lambda, theta) {
  N <- rpois(1, lambda * t)
  X <- rexp(N, rate = theta)
  sum(X)
}

ui <- fluidPage(
  titlePanel("Compound Poisson Process: S(t)"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("t", "Time (t):", value = 10, min = 1),
      sliderInput("lambda", "Interarrival rate (λ):", 
                  min = 0.1, max = 5, value = 1),
      sliderInput("theta", "Exponential rate of Xᵢ (θ):", 
                  min = 0.1, max = 5, value = 1),
      numericInput("nsim", "Number of simulations:", 
                   value = 1000, min = 100)
    ),
    
    mainPanel(
      plotOutput("histPlot"),
      verbatimTextOutput("summaryText")
    )
  )
)

server <- function(input, output) {
  
  output$histPlot <- renderPlot({
    sims <- replicate(input$nsim, simulate_S(
      input$t, input$lambda, input$theta
    ))
    hist(sims, main = paste("Histogram of S(t), t =", input$t),
         xlab = "S(t)", breaks = 30, col = "lightblue")
  })
  
  output$summaryText <- renderPrint({
    sims <- replicate(input$nsim, simulate_S(
      input$t, input$lambda, input$theta
    ))
    summary(sims)
  })
}

shinyApp(ui, server)
