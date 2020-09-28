#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  h1('Exploring the normal distribution'),
  hr(style="border-color:orange;"),
  
  withMathJax('The normal distribution has two parameters: \\(\\mu\\) and \\(\\sigma\\) 
              and is usually indicated as \\(N(\\mu,\\sigma)\\). Is this application you
              can explore several features of the normal distribution. For instance, given a 
              value of k, the probabiliy of observing a value within the interval
              \\(\\mu \\pm k \\times \\sigma  \\) is always the same for any normal, independently
              of the parameter values. In when \\( k=1.96 \\) that probability is 0.95.'),
  br(),
  withMathJax('The area under two values of X indicates the probability of obtaining
              values within this two values. For example, if the area is equal to 0.69,
              this meants tha if we observe a subject from this population
              its measured value of X will be within this interval in the 69% of the cases.'),
  br(),
  withMathJax('A sample of individuals will show a distribution (histogram) that is
              in accordance with the normal curve that represents the behaviour of this
              variable in the population.'),
  
 hr(style="border-color:orange;"),
  
  column(3,
         wellPanel(
         h3('Parameters'),
         numericInput('mu','Population mean \\((\\mu)\\)',value=100,step=0.1),
         numericInput('sigma','Population standard deviation \\((\\sigma)\\)',value=10,step=0.1)
         )
         )
         ,
  column(9,
         navbarPage('Normal distribution',
           tabPanel('Introduction',
                    withMathJax('In a normal distribution \\(N(\\mu,\\sigma)\\), the parameter
                                \\(\\mu\\) defines the position of the distribution. This
                                distribution is symmetric with respect \\(\\mu\\).That is:
                                \\(P(X>\\mu)=P(X<\\mu)=0.5\\).
                                The parameter \\(\\sigma\\) indicates the amplitude of the possible
                                values of X. The arrow in the graphic show the value of this
                                parameter fixed in the parameters panel. For a fixed value of
                                \\(k\\), the probability of the interval \\(\\mu \\pm k \\times \\sigma  \\) 
                                is always the same, independently of the parameter values. The probability
                                of obtaining a value offside the interval \\(\\mu \\pm 3 \\times \\sigma \\)
                                is almost 0.'),
                    hr(),
                    fluidPage(
                      column(4,
                             p("\\(P(\\mu - k \\times \\sigma < X < \\mu + k \\times \\sigma )\\)"),
                             radioButtons("kopt", "Value of k",
                                          choices = c('1' = 1,
                                                      '1.28'=2,
                                                      '1.96' = 3,
                                                      '2'=4,
                                                      '3'=5
                                          ),
                                          selected = 1) 
                      ),
                      
                      column(8,
                             sliderInput('xRange','Range of X axis',value=c(80,120),min=0,max=150),
                             numericInput('yRange','Max Y value',value=0.1,min=0,max=2,step=0.01),
                             plotOutput('plot.normal'))
                    )
           ),
            tabPanel('Computing probabilities',
                     tabsetPanel(
                       tabPanel('With R',
                                br(),br(),
                                withMathJax('If \\(X\\) is a \\(N(\\mu,\\sigma), then:\\)'),
                                br(),br(),
                                withMathJax('The probability \\(P(X<x)\\) is obtained with the
                                     instruction:'),
                                h3(code('pnorm(x,mu,sigma)')),
                                hr(),
                                withMathJax('The probability \\(P(X>x)\\) is obtained with the
                                     instruction:'),
                                h3(code('1-pnorm(x,mu,sigma)')),
                                hr(),
                                withMathJax('The probability of an interval \\(P(a<X<b)\\) is obtained with the
                                     instruction:'),
                                h3(code('pnorm(b,mu,sigma)-pnorm(a,mu,sigma)')),
                                hr(),
                                withMathJax('The value of \\(a\\) that fulfils \\(P(X<a)=\\alpha\\) is obtained with the
                                     instruction:'),
                                h3(code('qnorm(alpha,mu,sigma)')),
                                ),
                       tabPanel('Examples',
                          fluidPage(
                            column(3,
                                   numericInput('a','a',value=90,step=0.1,width=100),
                                   numericInput('b','b',value=105,step=0.1,width=100),
                                   radioButtons('optAreas','Select an option:',
                                                  choices=c(
                                                  '\\(P(a<X<b)\\)'=1,
                                                  '\\(P(X<a)\\)'=2,
                                                  '\\(P(X<b)\\)'=3,
                                                  '\\(P(X>a)\\)'=4,
                                                  '\\(P(X>b)\\)'=5,
                                                  '\\(P(X<\\mu)\\)'=6)
                                                  )
                                   ),
                               column(9,plotOutput('plot.ab'))
                                   )
                       )
                  )),
           
           tabPanel('The normal standard N(0,1)',
                    h3('The normal standard: N(0,1)'),
                    hr(),
                    tabsetPanel(
                      tabPanel('Theory',
                               hr(),
                               h3('Computing probabilities'),
                               hr(),
                              fluidPage(
                                  column(6,
                                    HTML('If \\(X\\) is a normal distribution \\(N(\\mu,\\sigma)\\), the the variable:
                                           $$Z=\\frac{X-\\mu}{\\sigma} \\rightarrow N(0,1)$$'),
                                    HTML('The \\(N(0,1)\\) is called normal standard and is denoted by \\(Z\\)'),
                                    hr(),
                                    HTML('If \\(X\\) is a normal distribution \\(N(\\mu,\\sigma)\\), then:
                                           $$P(X\\le x)=P \\left( Z \\le \\frac{x-\\mu}{\\sigma}\\right)$$')
                                    ),
                                  column(6,
                                         h4('Example'),
                                         hr(),
                                         HTML("Let's \\(X\\) be a normal distribution \\(N(100,3)\\)"),
                                         hr(),
                                         HTML('Then:
                                           $$P(X\\le 102)=P \\left( Z \\le \\frac{102-100}{3}\\right) = 
                                              P(Z \\le 0.67)$$'),
                                         HTML('In R it is equivalent to compute:'),
                                         br(),br(),
                                         verbatimTextOutput('pX'),
                                         verbatimTextOutput('pZ')
                                  )
                              ),
                              hr(),
                              h3('Computing quantiles'),
                              hr(),
                              fluidPage(
                                column(6,
                                       HTML('The value  \\(x_\\alpha\\) that fulfils the equation
                                            $$P(X \\le x_\\alpha)=\\alpha$$
                                            is called <b> quantile \\(\\alpha\\)</b> .'),
                                       HTML('Using the standard normal:
                                            $$P(X \\le x_\\alpha)=P \\left( Z \\le \\frac{x_\\alpha-\\mu}{\\sigma} \\right)=\\alpha$$'),
                                       HTML('Then 
                                            $$z_\\alpha=\\frac{x_\\alpha-\\mu}{\\sigma} \\rightarrow x_\\alpha=\\mu+z_\\alpha \\sigma$$')),
                                column(6,
                                       h4('Example'),
                                       hr(),
                                       HTML("Let's \\(X\\) be a normal distribution \\(N(50,2)\\)"),
                                       hr(),
                                       HTML('The quantile 0.92 is computed as'),
                                       HTML('<code>qnorm(0.92,50,2)</code>, which gives a result of
                                            <code>52.81014</code>'),
                                       HTML('The quantile 0.92 for the standard normal, i.e. \\(z_{0.92}\\) is
                                            obtained as <code> qnorm(0.92)</code> which gives <code>1.405072</code>'),
                                       HTML('You can check that:'),
                                       code('50+1.405072*2=52.81014')
                                  
                                )
                              )
                      ),
                      tabPanel('Z-scores',
                               br(),br(),
                               img(src='zscores.png', align = "center"),
                               h5('Source: (http://en.wikipedia.org/wiki/File:Normal_distribution_and_scales.gif) '),
                               hr(),
                               fluidPage(
                                 column(5,
                                        h3('Z-scores'),
                                        hr(),
                                    withMathJax('The z-score of an observed value \\(x\\) of a \\(N(\\mu,\\sigma)\\) is:
                                           $$z=\\frac{x-\\mu}{\\sigma}$$'),
                                    hr(),
                                    withMathJax('If \\(\\bar X\\) is the mean of a sample, and \\(s\\) is
                                                    the correspondiong standard deviation, then the z-score is
                                                    obtained as:
                                                    $$z=\\frac{x-\\bar X}{s}$$')
                               
                                  ),
                                 column(5,
                                        h3('Interpretation'),
                                        hr(),
                                        HTML('The z-score indicates how many times the observed value \\(x\\) is
                                             above (or below in case it is negative) the mean in units of 
                                             standard deviations.'),
                                        br(),br(),
                                        HTML('As z-scores are distributed as a \\(N(0,1)\\), its value can be
                                             read in terms of this distribution. For example, supose a man that has a weight of
                                             90.3 kg. In the population, we have a mean of 80.3kg. with a standard deviation of
                                             5.3kg. In tha case, the z-socre for this man is:
                                             $$ z =\\frac{90.3-80.3}{5.3}=1.89$$
                                             In a \\(N(0,1)\\) the \\(P(Z<1.89)=0.97\\) this means this man has a
                                             percentile of weigth of 97 and that few people on this population will
                                             have a higher weight.'),
                                        br(),br(),
                                        HTML('Z-scores allow comparing variables with different measurement units. For example,
                                             if an individual has a z-score of 2.1 for weight and 0.3 for height, this indicates
                                             that is close to the mean in height but well above the mean in weight!')
                                        ))
                        
                      ))
                    ),
           
            tabPanel(HTML('Sampling from a <br/> normal distribution'),
                     fluidPage(
                       column(12,
                              fluidRow(
                                column(3,numericInput('n','n',value=30,step=1,width=100)),
                                column(3,
                                       numericInput('bin','banwidth',value=3,step=.1,width=100),
                                       actionButton('go','New data',
                                                    style = "color: white; 
                                                     background-color: blue; ",
                                                    icon=icon('play')),
                                       checkboxInput('points','Show points',value=TRUE)),
                                column(3,radioButtons("dcurve", "Add normal curve?",
                                                      choices = c(No = FALSE,
                                                                  Yes = TRUE
                                                      ),
                                                      selected = FALSE)),
                                column(3,radioButtons("estcurve", "Add estimated curve?",
                                                      choices = c(No = FALSE,
                                                                  Yes = TRUE
                                                      ),
                                                      selected = FALSE))
                              ),
                              fluidRow(
                                column(6,plotOutput('histogram')),
                              column(6,plotOutput('boxplot'))
                              ) 
                       )
                     )
                    ),
           tabPanel('Check normality',
                    fluidPage(
                      column(9,checkboxInput("qqline", "Show QQ line?",TRUE),
                               plotOutput('qq'),
                             hr(),
                             verbatimTextOutput('NormalityTest')
                              )
                              )
                        )
                      )
                    ),

   column(12,
          hr(),
          HTML('(c) Albert Sorribas, Ester Vilaprinyo, Rui Alves, Montserrat Rue <br>
         Biomodels Group <br>
         University of Lleida - Institute of Biomedical Research (IRBLleida)'))
   ) 
          )
 
  


 
  

