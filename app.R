# #
# # This is a Shiny web application. You can run the application by clicking
# # the 'Run App' button above.
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/


library(shiny)
library(shinyjs)
znp1=function(n,prob,z){
  znp1=0
  if(z[n]==0){
    znp1=0
  }
  else{
    for(i in 1:z[n]){
      znp1=znp1+rgeom(n=1,prob=prob)
    }
  }
  return(znp1)
}
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
                                  max = 500,
                                  value = 1),
                      sliderInput("it",
                                  "Iterations",
                                  min = 2,
                                  max = 100,
                                  value = 10),
                      actionButton("regenerate", "Re-generate", icon = icon("sync"), style="color: #fff; background-color: #28a745; border-color: #28a745;"),
                    )),
                
                # Show a plot of the generated distribution
                mainPanel(
                  tabsetPanel(
                    # tabPanel("Description", p(".", style = "font-family: 'times'; font-si16pt")),
                    tabPanel("One iteration", plotOutput("plot1")),
                    tabPanel("Multiple iterations", plotOutput("plot2")),
                    tabPanel("Log", plotOutput("plot3"))
                  )
                )
  )
)

# Define server logic required to draw plot
server <- function(input, output) {
  randomsample <- reactive({
    input$regenerate
    #setup
    z=vector()
    z[1]=input$initial
    
    # generate an "empty" matrix
    samp <- matrix(NA, input$m, input$it)
    # simulation
    for (j in 1:input$it) {
      for(i in 1:(input$m-1)){
        z[i+1]=znp1(i,input$p,z) # z vector elements GW process (ie rows)
      }
      (samp[,j] <- z) # assign z vector to columns of matrix
    }
    samp
  })
  output$plot1 <- renderPlot({

    # plot one iteration (ie columm) 
    plot(randomsample()[,1], type = "l", xlab = "time", ylab = "number", main = "One Iteration")
  })
  output$plot2 <- renderPlot({
    samp <- randomsample()
    
    # plot graph 1 to t with y-axis the range of Zn values
    plot(c(1,input$m), range(samp), type = "n", xlab = "time", ylab = "population", main = paste(input$it, " Iterations"))
    legend("topleft", lty = 2, legend = "average")
    for (i in seq_len(input$it)) {
      lines(samp[,i], col = i) # plot each iteration
    }
    
    # average of all iterations 
    mu <- rowMeans(samp)
    # plot as dashed line
    lines(mu, col = "black", lty = 2, lwd = 2)
  })
  output$plot3 <- renderPlot({
    samp <- randomsample()
    
    # plot graph 1 to t with y-axis the range of Zn values
    plot(c(1,input$m), c(-2,max(log(samp))), type = "n", xlab = "time", ylab = "log(population)", main = paste(input$it, " Iterations"))
    for (i in seq_len(input$it)) {
      lines(log(samp[,i]), col = i) # plot each iteration
    }
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
