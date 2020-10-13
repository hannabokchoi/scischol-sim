# This is a detailed annotated version of app.R.
# Credit to Sophie Jones for help.

library(shiny)

# this is a function that will be used later in code. It can be understood in context later.
znp1=function(n,prob,z){ # create function called znp1
  znp1=0 # starting with 0 initial offspring
  if(z[n]==0){ # if number of individuals at time n is 0
    znp1=0 # account for extinction event
  }
  else{
    for(i in 1:z[n]){ # otherwise for each subsequent element in vector
      znp1=znp1+rgeom(n=1,prob=prob) # give birth to random number of offspring independently
    }
  }
  return(znp1)
}
# Define UI for application that draws lines
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
  randomsample <- reactive({ # use reactive expression
    input$regenerate # reset button
    #setup
    z=vector() # create empty vector z
    z[1]=input$initial # initial number of infected at time 1. Default is 1.
    
    p <- 1/(input$gmean+1) # geometric parameter p in terms of mean 1-p/p
    
    # generate an "empty" matrix called samp
    samp <- matrix(NA, input$m, input$it)
    # simulation
    for (j in 1:input$it) { # for j in 1 to number of iterations
      for(i in 1:(input$m-1)){ # for i in 1 to max(time) - 1
        z[i+1]=znp1(i,p,z) # fill i+1^th element of z vector with sum of i independent random variables, znp1, defined above
      }
      (samp[,j] <- z) # assign z vector to columns of matrix (unique iterations)
    }
    
    # detection probability matrix
    detected <- matrix(rbinom(p=input$pdetect, n=prod(dim(samp)), size=samp) > 0, nrow=dim(samp)[1]) 
    ## matrix of truth values where detected[i,j]==T if rbinom has > 0 success out of the [i,j]th element (number of trials) of samp, iterating over every element of samp (row*coloumn number of times)
    ## i.e. matrix of truth values associated with each element of samp, where each element of samp is the number of trials and with success probability 1-p
    for (i in 1:ncol(samp)) { # for i in 1 to number of columns
      if(any(detected[,i])) { # the first time detection is true in each column
        j <- which.max(detected[,i]) # minimum row number at which detection is true
        samp[j:nrow(samp),i] <- samp[j,i] # replace subsequent elements of the column with the same value as when detection[,i]=T
      }
    }
    samp
  })
  output$plot1 <- renderPlot({

    # plot one iteration of samp
    plot(randomsample()[,1], type = "l", xlab = "time", ylab = "number", main = "One Iteration")
  })
  output$plot2 <- renderPlot({
    samp <- randomsample()
    
    # plot multiple iterations with y-axis the range of samp to account for randomness
    plot(c(1,input$m), range(samp), type = "n", xlab = "time", ylab = "population", main = paste(input$it, " Iterations"))
    for (i in 1:ncol(samp)) { # for loop across columns of samp
      if((samp[input$m,i]==samp[input$m-1,i])&(samp[input$m,i]>0)) { # if the last element is equal to the one before and it is not equal to 0
        j <- which.max(samp[,i]==samp[input$m,i]) # set j as the minimum position at which this value appears.
        ## NB THIS IS NOT DETECTION EVENT; IT IS THE TIME AT WHICH WE REACH ANY NUMBER THAT EQUALS THE NUMBER AT DETECTION 
        # rare case where j is not detection event. I will call j detection event from now
        lines(x=c(1:j), y=samp[1:j,i], col = rgb(0,0.2,1,0.8)) # draw filled in lines from 1 to detection
        lines(x=c(j:nrow(samp)),y=samp[j:nrow(samp),i], col = rgb(0.2,0.4,1,0.8), lty = 3, lwd=2) # change colour and lty of line after detection
      }
      else{
        lines(samp[,i], col = rgb(0.6,0.2,1,0.8)) # otherwise extinction events are coloured purple (or all events purple if no detection)
      }
    }

    # average of all iterations 
    mu <- rowMeans(samp)
    # plot as dashed line if no detection
    if(input$pdetect==0){
    legend("topleft", lty = 2, legend = "average")
    lines(mu, col = "black", lty = 2, lwd = 2)
    }
    else{ # otherwise just have a legend that specifies what colours and lines represent.
      legend("topleft", lty=c(1,3), lwd=3, col = c(rgb(0.6,0.2,1,0.8),rgb(0.2,0.4,1,0.8)), legend = c("extinction","detected"))
    }
  })
  output$plot3 <- renderPlot({
    samp <- randomsample()
    
    # plot of the log of population if we do not have detection.
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
