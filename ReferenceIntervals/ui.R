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
shinyUI(
    
    fluidPage(
        
        
        column(12,
            navbarPage('Reference intervals',
            tabPanel('Concepts',
                     h2('Basic concepts for reference intervals'),hr(),
                     fluidPage(
                         column(4,
                         radioButtons('optConcept','Select a concept',
                                  choices=list(
                                      'Reference interval definition'=1,
                                      'Ref.int. in a normal distribution'=2,
                                      'Ref.int. from a sample'=3,
                                      'Ref.int. from quantiles'=4,
                                      'Ref.int. from boostrap'=5),
                                  selected=1
                                  ),
                         hr(),
                         h4('See more information'),
                         actionButton(inputId='wiki', 
                                      label="Reference range (wiki)", 
                                      icon = icon("th"), 
                                      onclick ="window.open(
                                    'https://en.wikipedia.org/wiki/Reference_range', 
                                    '_blank')",
                                      style="color: Black; background-color: Orange; border-color: #2e6da4"
                         )
                         ),
                         column(8,
                            wellPanel(style = "background: #F0FFFF",
                                      uiOutput('Formulas'))
                         )
                     ),
                     hr(),
                     h3('Some useful links'),
                     hr(),
                     
                     actionButton(inputId='wiki2', 
                                  label="Reference ranges for blood tests (wiki)", 
                                  icon = icon("th"), 
                                  onclick ="window.open(
                                    'https://en.wikipedia.org/wiki/Reference_ranges_for_blood_tests', 
                                    '_blank')",
                                  
                                  style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
                                    ),
                     actionButton(inputId='wiki2', 
                                  label="Article", 
                                  icon = icon("th"), 
                                  onclick ="window.open(
                                    'https://reader.elsevier.com/reader/sd/pii/S1532046415002907?token=29C9AB5C1C19003538258E9521058BA5241402D94E9DA5885B5A1188AB9218A6FA4596657C9CE321232A043B5D782970', 
                                    '_blank')",
                                  style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
                     ),
                     hr(),
                     wellPanel(style = "background: white",
                         h4('Example of reference intervals for creatinine as a function of age and sex'),
                         hr(),
                         img(src='Creatinin.jpg',width=500),
                            h5('den Elzen, W., Brouwer, N., Thelen, M., et al. (2018). 
                            NUMBER: standardized reference intervals in the Netherlands using a big data approach. 
                            Clinical Chemistry and Laboratory Medicine (CCLM), 57(1), pp. 42-56.
                            Retrieved 14 Nov. 2019, from doi:10.1515/cclm-2018-0462'))
                     ),
                     tabPanel(HTML('Compare methods for <br/> obtaining refrence intervals'),
                              h2('Comparison of methods for obtaining a reference interval'),
                              hr(),
                              p(HTML('As presented in the <b>Concepts</b> section, there are different methods for obtaining
                                     a reference interval. In this section, you can explore the results on samples of a normal
                                     distribution defined by its mean and sd. and compare the theoretical reference interval with
                                     the estimated intervals in the different methods.')),
                              hr(),
                     fluidPage(
                         column(3,
                                wellPanel(h3('Population parameters'),
                                          hr(),
                                          numericInput('m','Mean',value=100),
                                          numericInput('s','Standard deviation',value=5),
                                          numericInput('n','Sample Size',value=30)
                                )
                         ),
                         column(4,
                                actionButton('goCompare','New sample',icon=icon('list-alt'),
                                  style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                 hr(),
                                 p('A sample is randomly simulated from a normal distribution with the selected parameters.
                                   The reference interval are computed by three methods.'),
                                 hr(),
                                wellPanel(
                                 h4('Reference interval for the original distribution'),
                                 hr(),
                                 tableOutput('Original1')
                                 ),
                                 wellPanel(
                                 h4('Reference intervals computed from the sample'),
                                 hr(),
                                 tableOutput('Compare')
                                 
                                 )
                         ),
                         column(5,
                                checkboxInput('Densi.Normal','Add original distribution?',value=FALSE),
                                checkboxInput('Densi.Estimated','Add estimated distribution?',value=FALSE),
                                numericInput('bw','Binwidth:',value=5,step=.1,width=80),
                                hr(),
                                h4('Sample summary statistics'),
                                verbatimTextOutput('summary'),
                                plotOutput('Histo')))
                     ),
            tabPanel(HTML('Parametric estimate <br/> of a reference interval'),
                     fluidPage(
                         h3('Compute the reference interval from a sample of a normal distribution'),
                         hr(),
                         p(HTML('Here we consider you can indicate the mean and standard deviation of a sample for a 
                         biomarker of interest. Then, you can compute the corresponding reference interval using the
                         Student\'s method. The results also show the 95% confidence
                            interval of the reference interval limits. In the figure, we show thereference interval by
                            a red line and the confidence interval for each limit in blue. <b>In this case, we
                           assume the sample comes from a normal distribution.</b>')),
                         hr(),
                         column(3,
                                numericInput('obsmu','Sample mean:',value=123.2),
                                numericInput('obssd','Sample s.d.:',value=4.5),
                                numericInput('obsn','Sample size:',value=30),
                                actionButton('goRefInt','Compute ref.int.',icon=icon('list-alt'),
                                             style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                         column(4,
                                h4('Estimated reference interval'),
                                tableOutput('RefInt'),
                                hr(),
                                h4('95% confidence interval for each limit of the reference interval'),
                                tableOutput('RefInt.CI')),
                         column(5,
                                numericInput('xminRI','Scale biomarker min.',value=100),
                                numericInput('xmaxRI','Scale biomarker max.',value=140),
                                checkboxInput('CIYN','Draw CI for each limit?',value=FALSE),
                                plotOutput('PlotRefInt'))
                     )),
            tabPanel(HTML('Read data from an external file <br/> and select a variable for <br/>
                             computing its reference interval'),
                        h2(HTML('Read an <b>EXCEL</b> file and compute the reference interval of one variable')),
                        hr(),
                        p(HTML('You can select an <b>EXCEL</b> file by navigating thorough your file system.
                                Then, you can visualize the variables in that file, select one and obtain
                                the reference interval. As a disgnostic tool, we include the histogram of the
                                selected variable an a QQ-plot for evaluating if the normal distribution is
                                an appropriate model for that variable.')),
                        p(HTML('If the sample shows a clear non-normal aspect in its distribution, the Student\'s
                        method results are not valid. In that case the CI of the ref.int. limits are also non valid.')),
                     hr(),
                        fluidPage(
                            
                                #Create file input button 
                                column(5,
                                       fileInput(inputId = "file",
                                          label = "Chose a file:",
                                          accept=c('.xls','.xlsx'),
                                          multiple = FALSE),
                                
                                #Create action button ----
                                actionButton(inputId = "go", 
                                             label = "View variable names ...",
                                             icon = icon("th"), 
                                             style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                #tableOutput('text1'),
                                
                                verbatimTextOutput('text3'),
                                hr(),
                                #Final text output ----
                                
                                uiOutput('varChoice'),
                                h5('First cases of the selected variable...'),
                                fluidPage(
                                    column(4,
                                           tableOutput("text2")),
                                    column(7,
                                           verbatimTextOutput('resumVars'))
                                )
                            ),
                            column(7,
                                   h4(textOutput('VarSelected')),
                                   hr(),
                                   radioButtons('TypePlot','Select a plot:',
                                                choices=c('Histogram'=1,'Normality'=2),
                                                selected=1),
                                   numericInput('bwFile','Binwidth for histogram:',value=10),
                                   plotOutput('FilePlot'),
                                   hr(),
                                   fluidPage(
                                       column(6,
                                               h4('Reference intervals'),
                                               hr(),
                                               
                                               tableOutput('FileRI')),
                                       column(6,
                                              h4(HTML('Confidence intervals for <br/> the ref.int computed <br/>
                                              by the student\'s method'),
                                                 tableOutput('FileCI'))
                                       )
                                   )
                            )
                         
                        )
                
            ),
            hr(),
            h4('(c) Albert Sorribas 2019'),h5('Department of Basic Medical Sciences, 
               Biomedical Research Institute of Lleida (IRBLleida), Universitat de Lleida')
            )
    )
    )  
)
