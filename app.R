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

znp1=function(n,prob,z){ # create function
  znp1=0 # starting with 0 offspring
  if(z[n]==0){ 
    znp1=0 # account for extinction event
  }
  else{
    for(i in 1:z[n]){ # otherwise give birth to random number of offspring independently
      znp1=znp1+rgeom(n=1,prob=prob)
    }
  }
  return(znp1)
}
# Define UI for application 
ui <- fluidPage(
  
  # Application title
  titlePanel("Galton-Watson Process"),
  
  # Sidebar with a slider inputs
  sidebarLayout(div(id = "gwsim",
                    sidebarPanel(
                      sliderInput("m",
                                  "Time",
                                  min = 1,
                                  max = 30,
                                  value = 8),
                      sliderInput("gmean",
                                  "Geometric Mean",
                                  min = 0,
                                  max = 10,
                                  value = 2),
                      sliderInput("initial",
                                  "Initial population",
                                  min = 0,
                                  max = 100,
                                  value = 1),
                      sliderInput("it",
                                  "Iterations",
                                  min = 2,
                                  max = 50,
                                  value = 10),
                      sliderInput("pdetect",
                                  "Probability of Detection (1-p)",
                                  min = 0,
                                  max = 1,
                                  step = 0.001,
                                  value = 0.05),
                      actionButton("regenerate", "Re-generate", icon = icon("sync"), style="color: #fff; background-color: #28a745; border-color: #28a745;"),
                    )),
                
                mainPanel(
                  tabsetPanel(
                    # tabPanel("Description", p(".", style = "font-family: 'times'; font-si16pt")),
                    tabPanel("One iteration", plotOutput("plot1")),
                    tabPanel("Multiple iterations", plotOutput("plot2")),
                    tabPanel("Log (for detection probability 0)", plotOutput("plot3"))
                  )
                )
  )
)

# Define server logic required to draw plot
server <- function(input, output) {
  randomsample <- reactive({
    input$regenerate # reset button
    #setup
    z=vector() # create empty vector
    z[1]=input$initial # initial number of infected
    
    p <- 1/(input$gmean+1)
    
    # generate an "empty" matrix
    samp <- matrix(NA, input$m, input$it)
    # simulation
    for (j in 1:input$it) {
      for(i in 1:(input$m-1)){
        z[i+1]=znp1(i,p,z) # z vector elements GW process 
      }
      (samp[,j] <- z) # assign z vector to columns of matrix
    }
    
    # detection probability matrix
    detected <- matrix(rbinom(p=input$pdetect, n=prod(dim(samp)), size=samp) > 0, nrow=dim(samp)[1]) 
    for (i in 1:ncol(samp)) {
      if(any(detected[,i])) { # if detection is true
        j <- which.max(detected[,i])
        samp[j:nrow(samp),i] <- samp[j,i]
      }
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
    for (i in 1:ncol(samp)) {
      if((samp[input$m,i]==samp[input$m-1,i])&(samp[input$m,i]>0)) {
        j <- which.max(samp[,i]==samp[input$m,i])
        lines(x=c(1:j), y=samp[1:j,i], col = rgb(0,0.2,1,0.8))
        lines(x=c(j:nrow(samp)),y=samp[j:nrow(samp),i], col = rgb(0.2,0.4,1,0.8), lty = 3, lwd=2)
      }
      else{
        lines(samp[,i], col = rgb(0.6,0.2,1,0.8))
      }
    }

    # average of all iterations 
    mu <- rowMeans(samp)
    # plot as dashed line if no detection
    if(input$pdetect==0){
    legend("topleft", lty = 2, legend = "average")
    lines(mu, col = "black", lty = 2, lwd = 2)
    }
    else{
      legend("topleft", lty=c(1,3), lwd=3, col = c(rgb(0.6,0.2,1,0.8),rgb(0.2,0.4,1,0.8)), legend = c("extinction","detected"))
    }
  })
  output$plot3 <- renderPlot({
    samp <- randomsample()
    
    # plot graph 1 to t with y-axis the range of Zn values with log of population.
    if(input$pdetect==0){
    plot(c(1,input$m), c(-2,max(log(samp))), type = "n", xlab = "time", ylab = "log(population)", main = paste(input$it, " Iterations"))
    for (i in seq_len(input$it)) {
      lines(log(samp[,i]), col = i) # plot each iteration
    }
    }
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
