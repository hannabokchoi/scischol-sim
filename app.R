# #
# # This is a Shiny web application. You can run the application by clicking
# # the 'Run App' button above.
# #
# # Find out more about building applications with Shiny here:
# #
# #    http://shiny.rstudio.com/
# #
# # see https://gitlab.com/snjnz/326sim.git for example
# 
# library(s20x)
# library(shiny)
# 
# # Define UI for application that draws a histogram
# ui <- fluidPage(
# 
#     # Application title
#     titlePanel("Old Faithful Geyser Data"),
# 
#     # Sidebar with a slider input for number of bins 
#     sidebarLayout(
#         sidebarPanel(
#             sliderInput("bins",
#                         "Number of bins:",
#                         min = 1,
#                         max = 50,
#                         value = 30)
#         ),
# 
#         # Show a plot of the generated distribution
#         mainPanel(
#            plotOutput("distPlot")
#         )
#     )
# )
# 
# # Define server logic required to draw a histogram
# server <- function(input, output) {
# 
#     output$distPlot <- renderPlot({
#         # generate bins based on input$bins from ui.R
#         x    <- faithful[, 2]
#         bins <- seq(min(x), max(x), length.out = input$bins + 1)
# 
#         # draw the histogram with the specified number of bins
#         hist(x, breaks = bins, col = 'darkgray', border = 'white')
#     })
# }
# 
# # Run the application 
# shinyApp(ui = ui, server = server)

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# see https://gitlab.com/snjnz/326sim.git for example

library(s20x)
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Birth-death process"),
    
    # Sidebar with a slider input for probabilities
    sidebarLayout(
        sidebarPanel(
            sliderInput("p",
                        "Probability of death",
                        min = 0,
                        max = 1,
                        value = 0.4),
            sliderInput("q",
                        "Probability of birth",
                        min = 0,
                        max = 1,
                        value = 0.3),
            sliderInput("x",
                        "Initial",
                        min = 1,
                        max = 200,
                        value = 1)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Description", p("'Fixed' is using set.seed so will output the same result for the same probability. 'Random' will be different every time. I'm still not sure how to put a description into the correct place so will put here for now. Starting with at least one individual, probability of stepping down one is em(p) and stepping up two is em(q), and remains with probability em(1-p-q). This is a random walk using a sample with replacement. I am not really sure what to do about the part where it goes below zero... The simulation tab is just an attempt at simulating.", style = "font-family: 'times'; font-si16pt")),
                tabPanel("Fixed", plotOutput("plot1")),
                tabPanel("Random", plotOutput("plot2")),
                tabPanel("Simulation", plotOutput("plot3"))
            )
        )
    )
)

# Define server logic required to draw plot
server <- function(input, output) {
    
    output$plot1 <- renderPlot({
        # generate probabilities based on predefined input.
        set.seed(10)
        # starting from 1, move down 1 with probability p, up two with q and stay with probability 1-p-q. 
        RWalk1 <- cumsum(c(input$x,sample(c(-1,0,2),size=250,replace=T,prob=c(input$p,1-input$p-input$q,input$q)),0))
        
        # draw the plot with the specified probabilities 
        plot(RWalk1, type="l", ylab = "Population", xlab = "Time", main = "fixed")
    })
    output$plot2 <- renderPlot({
        RWalk2 <- cumsum(c(input$x,sample(c(-1,0,2),size=250,replace=T,prob=c(input$p,1-input$p-input$q,input$q)),0))
        
        # draw the plot with the specified probabilities 
        plot(RWalk2, type="l", ylab = "Population", xlab = "Time", main = "Random")
    })
    output$plot3 <- renderPlot({
        samp <- matrix(sample(c(-1,0,2),size=4000,replace=T,prob=c(input$p,1-input$p-input$q,input$q)), nrow = 10)
        samp = cbind(input$x, samp, 0)
        Rwalks <- t(apply(samp, 1, cumsum))
        
        plot(c(1,400), range(Rwalks), type = "n", ylab = "Population", xlab = "Time")
        for (i in seq_len(10)) {
            lines(Rwalks[i,], col = i)
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
