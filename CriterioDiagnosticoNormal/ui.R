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
  navbarPage('Diagnostic criteria based on biomarkers',
  tabPanel('Concepts',icon=icon('info-circle'),
           fluidPage(
             column(3,
                    radioButtons("OptConcept", label = h3("Select a concept"), 
                                       choices = list("Sensitivity and Especificity" = 'Sen', 
                                                      "Interpretation in biomarkers" = 'Norm',
                                                      "ROC curve"='ROC',
                                                      "Predictive values"='PV'),
                                       selected = 'Sen')),
             column(9,
             align='center',
             uiOutput('Concepts'))
             ),
             ),
  tabPanel(HTML('Test properties: TPR, TNR <br/>
                FDR, PPV, NPV..etc.'),  icon=icon('procedures'),
      fluidPage(
      h1('Requirements for an optimal diagnostic criteria'),
      hr(),
      p('For a given sensitivity and specificity, the utility of a diagnostic criteria depends on the prevalence of the disease.
        Here you can explore this problem. In the following, H stands for Healthy and D for Disease. 
        (+) Indicates a positive result of the test
        and (-) a negative result. P(+/H) is the probability of a positive test amnog healthy people, etc.'),
      hr(),
      column(3,
             numericInput('Sensi','Sentitivity P(+/D):',value=0.8,step=0.01,min=0,max=1),
             numericInput('Espe','Especificity (P(-/H):',value=0.8,step=0.01,min=0,max=1),
             hr(),
             numericInput('Prev','Prevalence of disease:',value=0.3,step=0.01,min=0,max=1)),
      column(8,
             tableOutput('Performance'))),
      hr(),
      fluidPage(
      column(6,
             plotOutput('PlotPV')
             ),
      column(6,
             h3('Predictive value of the test'),
             p('The red line indicates the Positive Predictive Value (PPV). The blue line
               indicates the Negative Predictive value (NPV). You can move the prevalence
               for evaluating the utility of the test with the indicated sensitivity and
               especificty. Also, for a fixed value of the prevalence, you can explore
               the required sensitivity and especificity for attaining useful predictive values.'),
             p('Both the PPV and the NPV should be greater that 0.5. The horizontal black line at
               0.5 indicates this limit. You should explore the validity of a test with a given
               sensitivity and especificity for matching this requirement for different prevalences.'),
             p('Low prevalences correspond to cases in which the disease affects few people. In that
               case, it is easy to attain high NPV. However, it will be difficult to achieve high PPV.
               The reverse occurs when a disease is present with high prevalence.'),
             hr())
      )),
      
      
       
  tabPanel(HTML('Test based on a biomarker <br/> with normal distributions <br/>
                ROC curve and test properties'), icon=icon('tree'),
           fluidPage(
             column(12,
                    # Application title
                    h1("Criteria for diagnostic based on normal distributions",color='blue'),
                    hr(),
                    p('In this exercise, we explore the effect of selecting a given value of a biomarker for discriminating between healthy and disease people.
          You can move the diagnostic point and see the resulting sensitivity and especificity. It is important to note that increasing
          sensitivity diminishes especificity and viceversa. The distribution of the biomarker in each population is determinant in
          the performance of any criteria. The ROC curve evaluates the discriminant ability of this biomarker in a given scenario.
          The AUC measures the performance in each case. An AUC=0.5 indicates that the choosen biomarker performs as toosing a money.
          The optimal discrimination is attained for high values of AUC (closed to 1).')
             ),
             column(2,
                    wellPanel(h3('Healthy',style="color:blue"),
                              numericInput("mu.s",
                                           "Mean:",
                                           value = 10,
                                           step=0.1),
                              numericInput("sd.s",
                                           "sd:",
                                           value = 3,
                                           step=0.1)
                    ),
                    wellPanel(h3('Disease',style="color:red"),
                              numericInput("mu.e",
                                           "Mean:",
                                           value = 15,
                                           step=0.1),
                              numericInput("sd.e",
                                           "sd:",
                                           value = 3,
                                           step=0.1)
                    ),
                    numericInput("xc",
                                 "Diagnostic point:",
                                 value = 12,
                                 step=0.1)
                    
             ),
             
             # Show a plot of the generated distribution
             column(10,
                    wellPanel(
                      numericInput('xmin','X min:',value=0,step=1),
                      numericInput('xmax','X max:',value=30,step=1),
                      plotOutput("plot"),
                      fluidRow(
                        column(2,''),
                        column(5,h3(textOutput('sen'),style='color:red')),
                        column(5,h3(textOutput('esp'),style='color:blue'))
                      )),
                    wellPanel(plotOutput("ROC")),
                    hr()
                    
             )
           )),
  tabPanel('Empirical data analysis',icon = icon("bar-chart-o"),
           fluidPage(
           h1('Properties of a test from observed data')  ,
           hr(),
           p('Supose you have a test and apply it to two samples of healthy and disease
             people. In this exercise, you can compute the resulting sensitity and 
             especificty as a function of the point you define for defining positive
             and negative results. Data is generated randomly from two normal distributions.'),
           
           p('For simplicity, we will consider that the disease increases the value of the biomarkes, i.e. the mean value is
          higher for people suffering from that disease.'),
           hr(),
           column(3,
                  h4('Healthy population'),
                  numericInput('mh','Mean:',value=100,step=1,min=0),
                  numericInput('sdh','sd:',value=5,step=.1,min=0),
                  numericInput('nh','Sample size:',value=30,step=1,min=0),
                  h4('Disease population'),
                  numericInput('md','Mean:',value=105,step=1,min=0),
                  numericInput('sdd','sd:',value=5,step=.1,min=0),
                  numericInput('nd','Sample size:',value=30,step=1,min=0)
                  ),
           column(3,
                  numericInput('PointDiagnostic','Value for criteria',value=102,step=.1),
                  actionButton('GoSampleEmpiric','Generate a new sample',icon=icon('list-alt'),
                               style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                  ),
            
           column(3,
                  p('Based on the data, the (+) and (-) results of the test are computed according
                     to the diagnostic point defined.'),
                  tableOutput('TableEmpirica')),
            column(3,
                   h4(textOutput('SensiEmpirica')),
                   h4(textOutput('EspeEmpirica'))
                  ),
           hr(),
           fluidPage(
             column(4,
                  plotOutput('HistoEmpiric')),
             column(5,
                  plotOutput('ROCEmpiric'))
           )
                   
           )),
  
  tabPanel('Compare biomarkers',icon=icon('stethoscope'),
           h3('Compare tests based on two different biomarkers'),
           hr(),
           p(HTML('Data are randomly generated from samples of two biomarker values. In each case
                  the AUC is computed for assesing test performance. ')),
           hr(),
           fluidPage(
             column(3,
                    h4('Generate new samples'),
                    actionButton('newROC','New data:')
                    ),
             column(9,
                    plotOutput('SimulaROC')
             )
           )),
  
hr(),
h5('(c) Albert Sorribas 2019'),
h5('Department of Basic Medical Sciences'),
h5('Biomedical Research Institute of Lleida (IRBLleida)'),
h5('Universitat de Lleida')
))

  

