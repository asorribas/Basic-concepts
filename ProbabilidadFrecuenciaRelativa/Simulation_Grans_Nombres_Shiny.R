#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load required libraries

library(shiny)
library(epitools)
library(ggplot2)

# Functions

mostra <- function(n,p) {
  rbinom(n,1,p)
}
# Define UI 

ui <- fluidPage(
   
   # Application title
   titlePanel("Convergence of the relative frequency to probability"),
   hr(),
   h4(paste('Simulation of observing the relative frequency of an event of a given \n',
            'probability in a series of observations.\n',
            'The relative frequency tends to the probability for\n',
            'a large number of observations')),
   p(a("Law of great numbers",href='https://en.wikipedia.org/wiki/Law_of_large_numbers')),
   hr(),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("prob",
                     "Event probability:",
                     min = 0.01,
                     max = 1,
                     value = 0.5),
         numericInput("n",
                     "Number of total observations:",
                     min = 2,
                     max = 1000,
                     value = 50),
         br(),
         actionButton("go", "Generate observations"),
         br(),
         br(),
         h4('LEGEND'),
         tags$b(span(p('Blue line:'),style='color:blue')),         
         p(paste('Prediction 95% interval where we expect to find \n',
                 'the relative frequency with the especified number of events.\n')),
         tags$b(span(p('Red line:'),style='color:red')),
         p(paste('True probability. Samples are obtained ramdonly \n','as events of that probability.\n')),
         tags$b(span(p('Black dots:'),style='color:black')),
         p(paste('Prediction 95% interval for the relative frequency \n',
                 'of the event at a given number of observations.\n')),
         tags$b(span(p('Empty dots:'),style='color:grey')),
         p(paste('Observed relative frequency at a given\n',
                 'number of observations.'))
         
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        wellPanel(plotOutput("plot")),
        wellPanel(span(h4(textOutput('text'),style='color:blue')),
        tableOutput('ic'))
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
   r <- eventReactive(input$go,{mostra(input$n,input$prob)})
   t <- eventReactive(input$go,{('Relative frequency obtained with the indicated number of observations and 95% prediction interval for the expected result.')})
   
   output$plot <- renderPlot({
     index <- 1:input$n
     csum <- cumsum(r())
     d <- data.frame(id=index,csum=csum/index)
     p <- input$prob
     inc <- 1.96*sqrt(p*(1-p)/index)
     
      ggplot(d,aes(x=id,y=csum))+
        geom_point(size=2,shape=21,fill='white')+
        geom_path()+
        scale_y_continuous(limits=c(0,1))+
        xlab('Number of observations')+
        ylab('Relative frequency')+
        geom_hline(yintercept=p,color='red')+
        ggtitle(paste('Law of large numbers'))+
        theme(plot.title = element_text(hjust = 0.5,size=18))+
        theme(axis.title = element_text(hjust = 0.5,size=18))+
        geom_point(aes(x=index,y=p+inc))+
        geom_point(aes(x=index,y=p-inc))+
        geom_hline(yintercept=p+1.96*sqrt(p*(1-p)/input$n),color='blue')+
        geom_hline(yintercept=p-1.96*sqrt(p*(1-p)/input$n),color='blue')
         } 
     )
   
  
   
   output$ic <- renderTable({
     x <- cumsum(r())[[length(r())]]
     n <- input$n
     final <- x/n
     p <- input$prob
     inc <- 1.96*sqrt(p*(1-p)/n)
     d<-data.frame(Observations=n, Events=x,Rel.freq=final,Interval.low= p-inc,Interval.upper=p+inc)
     d
   })
   
   output$text <- renderText({
     r<-t()
     r})
}

# Run the application 
shinyApp(ui = ui, server = server)

