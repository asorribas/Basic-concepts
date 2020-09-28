#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram
shinyUI(
  
  navbarPage('Probability and relative frequency',
    tabPanel('Law of great numbers',
    fluidPage(
      # Application title
      h3("Convergence of the relative frequency to probability"),
      hr(),
    h4(paste('Simulation of observing the relative frequency of an event of a given \n',
             'probability in a series of observations.\n',
             'The relative frequency tends to the probability for\n',
             'a large number of observations')),
    p(a("Law of great numbers",href='https://en.wikipedia.org/wiki/Law_of_large_numbers')),
    hr(),
    
    # Sidebar with a slider input for number of bins 
    column(4,
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
        actionButton("go", "Generate observations",
                          style = "color: white; 
                         background-color: blue; 
                         ",
                     icon=icon('play-circle')
                     ),
         
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
    column(8,
      # Show a plot of the generated distribution
        wellPanel(plotOutput("plot")),
        wellPanel(span(h4(textOutput('text'),style='color:blue')),
                  tableOutput('ic'))
      )
    )
  ),
  tabPanel('Sampling',
           fluidPage(
             
             # Application title
             titlePanel("Distribution of relative frequency"),
             hr(),
             
             # Sidebar with a slider input for number of bins 
             fluidRow(
               
               column(3,
                      wellPanel(sliderInput("prob2",
                                            "Probability (p):",
                                            min = 0.01,
                                            max = 1,
                                            value = 0.5),
                                numericInput("n2",
                                             "Observations (n):",
                                             min = 2,
                                             max = 1000,
                                             value = 20)
                      )),
               column(3,
                      wellPanel(
                        numericInput("ns",
                                     "Number of samples:",
                                     value = 50),
                        sliderInput("xmin",
                                    "Scale x min:",
                                    min = 0,
                                    max = 0.4,
                                    value = 0),
                        sliderInput("xmax",
                                    "Scale x max:",
                                    min = 0.6,
                                    max = 1,
                                    value = 1),
                        sliderInput("binw",
                                    "Bin width:",
                                    min = .01,
                                    max = .05,
                                    value = .05)
                      )),
               column(6,
                      h3('Goal'),
                      p('Perform a simulation on the relative fequency of an event of a given probability.'),
                      p('The red lines in the figure indicates the expected limits for the relative frequency for the 95% of the samples with n observations in each sample.'),
                      p('You can indicate the value of probability and the number of observations by sample. Then, you can especify a number of samples and the scale of the figure.'),
                      p('In the results panel, the percentage of samples within the theoretical limits is shown. The proportion of samples within the limits shouls approach 0.95.'),
                      p('Try increasing the number of samples so that this proportion converges towards 0.95.')
               )
             ),
             fluidRow(
               column(4),
               column(3,actionButton("go2", "Generate samples",
                                     icon=icon('play-circle'),
                                     style="color: white; 
                            background-color: blue;")))
             ,
             hr(),
             
             # Show a plot of the generated distribution
             fluidPage(
               column(6,
                      fluidRow(plotOutput("plot2"))
               ),
               column(1),
               column(5,
                      fluidRow(h3('Parameters'),
                               tableOutput('tablepar')),
                      fluidRow(h3('Results'),
                               tableOutput('tableres'))
                      
               )
             )
             
             
             
           )),
  tabPanel('Binomial model',
           h3('The binomial model'),
           hr(),
           p('The binomial model allow for computing the probability of each of the possible results of observing how
             many times an event will occur in a given sample. The model assumes that in each observation the
             probability of the event is constant.'),
           hr(),
           fluidPage(
             column(3,
                    numericInput('piB','Value of the probability',value=0.5,min=0.01,max=0.99,step=0.01),
                    numericInput('nB','Size of sample',value=10,min=2,max=30,step=1))
             ),
           fluidPage(
             hr(),
             h3('Expected results'),
             hr(),
             column(5,
                    plotOutput('dBinomial')),
             
             column(2,
                    tableOutput('dBinomialTable')),
             
             column(5,
                    plotOutput('dBinomialFreq')))
           )
)
)
