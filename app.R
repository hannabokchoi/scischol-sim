
library(shiny)
library(shinyjs)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Galton-Watson Process"),
  
  # Sidebar with a slider input for probabilities
  sidebarLayout(div(id = "gwsim",
    sidebarPanel(
      sliderInput("m",
                  "Time",
                  min = 1,
                  max = 100,
                  value = 8),
      sliderInput("p",
                  "Probability",
                  min = 0,
                  max = 1,
                  value = 0.3),
      sliderInput("initial",
                  "Initial population",
                  min = 0,
                  max = 1000,
                  value = 0),
      sliderInput("it",
                  "Iterations",
                  min = 2,
                  max = 100,
                  value = 10),
      actionButton("regenerate", "Re-generate!", icon = icon("sync"), style="color: #fff; background-color: #28a745; border-color: #28a745;"),
    )),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        # tabPanel("Description", p(".", style = "font-family: 'times'; font-si16pt")),
        tabPanel("Single", plotOutput("plot1")),
        tabPanel("Geometric", plotOutput("plot2"))
        # tabPanel("Log", plotOutput("plot3"))
      )
    )
  )
)


# Define server logic required to draw plot
server <- function(input, output) {
  
  randomsample <- reactive({
    input$regenerate
    samp <- matrix(NA, input$m, input$it)
    samp[1,] <- input$initial
    for (i in seq_len(nrow(samp)-1)) {
      samp[i+1, ] <- samp[i,] + rgeom(n = input$it, prob = input$p)
    }
    samp
  })
  output$plot1 <- renderPlot({
    # #continuous time
    # znp1=function(n,prob){
    #   znp1=0
    #   if(z[n]==0){
    #     znp1=0
    #   }else{
    #     for(i in 1:z[n]){
    #         znp1=znp1+rgeom(n=1,prob=prob)
    #     }
    #   }
    #   return(znp1)
    # }
    # #setup
    # z=vector()
    # z[1]=input$initial
    # m=input$m
    # #simulation
    # for(i in 1:(m-1)){
    #   z[i+1]=znp1(i,input$p)
    # }
    plot(randomsample()[,1], type = "l", xlab = "time", ylab = "number")
  })
  output$plot2 <- renderPlot({
    samp <- randomsample()
    
    plot(c(1,input$m), range(samp), type = "n", xlab = "time", ylab = "population")
    for (i in seq_len(input$it)) {
      lines(samp[,i], col = i)
    }
    
    mu <- rowMeans(samp)
      lines(mu, col = "black", lty = 2, lwd = 2)
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
