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
    navbarPage('Events and their probability',
               tabPanel('Theory',
                        tabsetPanel(
                          tabPanel('Events',
                                   hr(),
                                   img(src='Venn.png',height="50%", width="50%", align="center"),
                                   br(),
                                   br(),
                                   HTML("Events are represented as sets, and this by 
                                        <a href='https://en.wikipedia.org/wiki/Venn_diagram' target='_blank'>Venn diagrams</a>. 
                                        It is interesting to remember some properties.<br>"),
                                   br(),
                                   
                                   HTML("<b>Union</b> \\(A\\cup B\\): when \\(A\\) or \\(B\\) or both are observed. In the example:"),
                                   withMathJax("$$(A \\cup B) = (A \\cap \\bar B) \\cup (A \\cap B) \\cup (\\bar A \\cap B)$$"),
                                   br(),
                                   HTML("<b>Intersection</b>\\(A\\cap B\\): when \\(A\\) and \\(B\\) are simultaneously observed."),
                                   br(),
                                   br(),
                                   HTML("<b>Morgan's laws</b>:"),
                                   HTML("$$(A \\cup B)^c = (A^c \\cap B^c)$$"),
                                   HTML("$$(A \\cap B)^c = (A^c \\cup B^c)$$"),
                                   br(),
                                   br(),
                                   HTML("<b>Inclusion</b> An event \\(A\\) is included in \\(B\\) when all observations of \\(A\\) are also observations of \\(B\\).
                                        $$A \\subset B$$"),
                                   HTML("Accordingly, $$(A \\cap B) \\subset (A \\cup B)$$")
                                   
                                   ),
                          tabPanel('Probability',
                                   br(),
                                   withMathJax("The probability of observing an event \\(A\\) is noted as \\(P(A)\\)."),
                                   hr(),
                                   withMathJax("$$0 \\leq P(A) \\leq 1$$"),
                                   hr(),
                                   withMathJax("If \\(A \\subset B\\), then $$P(A) \\leq P(B)$$"),
                                   hr(),
                                   withMathJax("The probabilty of observing \\(A\\) or  \\(B\\) is: $$P(A \\cup B)=P(A)+P(B)-P(A \\cap B)$$"),
                                   hr(),
                                   withMathJax("If \\((A\\cap B)=\\emptyset \\), then \\(P(A \\cap B)=0\\), thus: $$P(A \\cup B)=P(A)+P(B)$$"),
                                   hr(),
                                   withMathJax("The probability of the complementary of \\(A\\) is:"),
                                   withMathJax("$$P(\\bar A)=1-P(A)$$")
                                   ),
                           tabPanel('Conditional probability',
                                    br(),
                                    withMathJax("The probability of \\(A\\) given that \\(B\\) is present is defined as:"),
                                    withMathJax("$$P(A|B)=\\frac{P(A \\cap B)}{P(B)}$$"),
                                    withMathJax("In general \\(P(A|B) \\neq P(A)\\)."),
                                    withMathJax("From its definition, it follows that:"),
                                    withMathJax("$$P(A \\cap B) = P(A|B) P(B) = P(B|A) P(A)$$"),
                                    withMathJax("Thus:"),
                                    withMathJax("$$P(A|B)=P(B|A)\\frac{P(A)}{P(B)}$$"),
                                    hr(),
                                    withMathJax("If \\(P(A|B)=P(A)\\), then the two events are independent, as there is the same
                                                expectative for \\(A\\) indepndently of the presence of B. In that case:"),
                                    withMathJax("$$P(A \\cap B)=P(A)P(B)$$"),
                                    hr(),
                                    h4('Interpretation'),
                                    HTML("The probability of a heart stroke is not the same for males and females.<br>"),
                                    HTML("The probability of obesity is different for people taking different diets.<br>"),
                                    HTML("The probability of recovering from a health problem depends on age and genetic characteristics.")
                                    ),
                           tabPanel('Law of total probability',
                                hr(),
                                img(src='ProbTotal.png',height="50%", width="50%", align="center"),
                                hr(),
                                withMathJax('If we have a partition \\(A_1,A_2,...,A_k\\), and an event \\(B\\), then we can write:'),
                                withMathJax('$$B=(A_1\\cap B)\\cup (A_2 \\cap B) \\cup ... \\cup (A_k \\cap B)$$'),
                                withMathJax('Then the \\(P(B)\\) can be written as:'),
                                withMathJax('$$P(B)=P(A_1\\cap B)+ P(A_2 \\cap B) + ... +P(A_k \\cap B)$$'),
                                withMathJax('and finally:'),
                                withMathJax('$$P(B)=P(B|A_1) P(A_1)+ P(B|A_2) P(A_2) + ... +P(B|A_k) P(A_k)$$')
                                ),
                           tabPanel("Baye's theorem",
                                    br(),
                                    withMathJax("The Baye's theorem describes the probability of an event, based on prior knowledge of conditions that might be related to the event."),
                                    withMathJax("$$P(A|B)=\\frac{P(A \\cap B)}{P(B)}=\\frac{P(B|A) \\times P(A)}{P(B)}=\\frac{P(B|A) \\times P(A)}{P(B|A) \\times P(A)+P(B|\\bar A) \\times P(\\bar A)}$$"),
                                    hr(),
                                    h4('Interpretation'),
                                    withMathJax("For example, the probability of having a disease \\((D)\\) if the result of a laboratory test is positive (+) can be computed as:"),
                                    withMathJax("$$P(D|+)=\\frac{P(+|D)\\times P(D)}{P(+|D)\\times P(D)+P(+|H)\\times P(H)}$$"),
                                    withMathJax("We can compute this probability if we know the probability that a person that has the disease will give a positive result, i.e. \\(P(+|D)\\), the
                                    probability of a positive result for healthy people, i.e. \\(P(+|H)\\), and the prevalence of the disease \\(P(D)\\)")
                                    
                                    )
                       )),
    
               tabPanel(HTML('Bayes theorem and <br>probability of a disease'),
                        h3('Which is the probability of a disease \\(A\\) given a symptom \\(S\\)?'),
                        hr(),
                        withMathJax("We will use the Baye's theorem:"),
                        withMathJax("$$P(A|S)=\\frac{P(A\\cap S)}{P(S)}=\\frac{P(S|A)\\times P(A)}{P(S|A)\\times P(A)+P(S|\\bar A)\\times P(\\bar A)}$$"),
                        withMathJax("Thus we need to indicate the probability of observing the symptom if the person has the disease: \\(P(S|A)\\), and
                                    this probability for the healthy people: \\(P(S|\\bar A)\\). The probability of having the disease given the symptom
                                    depends also on the prevalence of the disease in the population \\(P(A)\\). The application computes \\(P(A|S)\\) for
                                    the selected probabilities and also the curve showing how this probability changes with the prevalence."),
                        hr(),
                        fluidPage(
                          column(2,
                                 sliderInput('pSA',withMathJax("\\(P(S|A)\\)"),value=0.5,min=0,max=1,step=0.01),
                                 sliderInput('pSAc',withMathJax("\\(P(S|\\bar A)\\)"),value=0.5,min=0,max=1,step=0.01),
                                 sliderInput('pA',withMathJax("\\(P(A)\\)"),value=0.5,min=0,max=1,step=0.01)),
                          column(6,
                                 plotOutput('Bayes')),
                          column(3,
                                 h4('A priori P(A)'),
                                 textOutput('BayesPrior'),
                                 h4('A posteriori P(A|S)'),
                                 textOutput('BayesPosterior')
                                 )
                        )
                        ),
               tabPanel(HTML('Diagnostic test'),
                       
                        h3('Diagnostic test: interpretation'),
                        hr(),
                        fluidPage(
                            column(3,
                                   numericInput('Sensib','Sensitivity: P(+|D)',value=0.8,min=0,max=1,step=0.01),
                                   numericInput('Espec','Especificity: P(-|H)',value=0.8,min=0,max=1,step=0.01),
                                   numericInput('Prev','P(D)',value=0.3,min=0,max=1,step=0.01)),
                            column(8,
                        tabsetPanel(
                            tabPanel('Theory',
                                     br(),br(),
                                    withMathJax("The simplest diagnostic test produce two possible outcomes: (+) and (-). The positive
                                                (+) result is associated with a diagnostic of presence of a disease condition, while a
                                                negative (-) result is associated with a healthy diagnostic."),
                                    hr(),
                                    HTML('<b>Sensitivity</b>: \\(P(+|D)\\) Is the probability of obtaining a positive result in
                                                the test on an individual that has the disease.'),
                                    hr(),
                                    HTML('<b>Especificity</b>: \\(P(-|H)\\) Is the probability of obtaining a negative result in
                                                the test on a healthy individual.'),
                                    hr(),
                                    HTML('<b>False positive rate (FPR)</b>: \\(P(+ \\cap H)\\) When the test is applied to a population
                                         where the prevalence of the disease is \\P(D)\\), the FPR is the proportion of
                                         subjects that will be healthy with a positive result.'),
                                    hr(),
                                    HTML('<b>False negative rate (FNR)</b>: \\(P(- \\cap D)\\) When the test is applied to a population
                                         where the prevalence of the disease is \\P(D)\\), the FNR is the proportion of
                                         subjects that will have the disease with a negative result.'),
                                    hr(),
                                    HTML('<b>FNR+FPR</b>: Indicates the probability of an erroneous diafnostic when
                                         applying the test on that population.')
                                    ),
                            
                            tabPanel('Using a diagnostic test',
                                     br(),br(),
                                     HTML("<b>Proportion of false positives (FPR)</b>: \\(P(+\\cap H)=P(+|H) \\times P(H)\\)"),
                                     textOutput('FPR'),
                                     hr(),
                                     HTML("<b>Proportion of false negatives (FNR)</b>: \\(P(-\\cap D)=P(-|D) \\times P(D)\\)"),
                                     textOutput('FNR'),
                                     hr(),
                                     HTML("<b>Proportion erroneous diagnostics</b>: \\(P(-\\cap D)+(P(+\\cap H)\\)"),
                                     textOutput('PDE'),
                                     hr(),
                                     h4('Simulation'),
                                     hr(),
                                     actionButton('goTest','New Sample',
                                                  style='background-color:orange',
                                                  icon('play-circle')),
                                     hr(),
                                     numericInput('nTest','Sample Size',value=100,min=0,step=10),
                                     h3(verbatimTextOutput('TableTest'))
                                     ),
                            tabPanel('Predictive value of a test',
                                     br(),br(),
                                     HTML("<b>Positive predictive value (PPV)</b>: \\(P(D|+)=
                                          \\frac{P(+|D)P(D)}{P(+|D)P(D)+P(+|H)P(H)}\\)"),
                                     textOutput('PPV'),
                                     hr(),
                                     HTML("<b>Negative predictive value (NPV)</b>: \\(P(H|-)=
                                          \\frac{P(-|H)P(H)}{P(-|H)P(H)+P(-|D)P(D)}\\)"),
                                     textOutput('NPV')
                                     )
                            )))
                   
               ),
               tabPanel(HTML('Which disease <br> is more probable'),
                        h3('Which disease is more probable based on symptoms?'),
                        hr(),
                        HTML('Supose you need to decide among four possible diseases \\(A_1, A_2, A_3, A_4\\), based on a symptom \\(S\\). <br>
                                Indicate your <i>a priori</i> probabilities for each disease and the probability of <br>
                                a symptom under each of the considered disease. The application computes the <i>posterior <br>
                                probabilities</i> of the diseases.'),
                        hr(),
                        
                        
                        fluidPage(
                            column(3,
                                   h4(HTML('<i>A priori</i> probabilities')),
                                   hr(),
                                   p('Which are the probabilities that you assignate to each disease (must add up to 1)?'),
                                   numericInput('pA1',withMathJax('\\(P(A_1)\\)'),value=0.25,step=0.01,min=0,max=1),
                                   numericInput('pA2',withMathJax('\\(P(A_2)\\)'),value=0.25,step=0.01,min=0,max=1),
                                   numericInput('pA3',withMathJax('\\(P(A_3)\\)'),value=0.25,step=0.01,min=0,max=1),
                                   numericInput('pA4',withMathJax('\\(P(A_4)\\)'),value=0.25,step=0.01,min=0,max=1)
                                   )
                        
                        ,
                        column(3,
                               h4('Probability of the symptom under each disease'),
                               hr(),
                               p('Which is the probability of the symptom (S) if a person suffers a given disease?'),
                               sliderInput('pBA1',withMathJax('\\(P(S|A_1)\\)'),value=0.25,step=0.01,min=0,max=1),
                               sliderInput('pBA2',withMathJax('\\(P(S|A_2)\\)'),value=0.25,step=0.01,min=0,max=1),
                               sliderInput('pBA3',withMathJax('\\(P(S|A_3)\\)'),value=0.25,step=0.01,min=0,max=1),
                               sliderInput('pBA4',withMathJax('\\(P(S|A_4)\\)'),value=0.25,step=0.01,min=0,max=1)
                               )
                        ,
                        column(6,
                               h4('Probability of observing the symptom in a person selected randomly in this situation:'),
                               withMathJax('\\(P(S)=P(S/A_1)\\times P(A_1)+..+P(S/A_k)\\times P(A_k)=\\)'),
                              h4(textOutput('PB')),
                              hr(),
                              h4(HTML('Probabilities <i>a posteriori</i> compared to the <i>a priori</i> information.')),
                              withMathJax('\\(P(A_i|S)=\\frac{P(S/A_i)\\times P(A_i)}{P(S)}\\)'),
                              tableOutput('posteriorTable'),
                              plotOutput('posterior'))
                        )
               ),
               tabPanel('Diagnostic based on two symptoms',
                        h3('Which is the probability of a disease given that symptom 1 or symptom 2 are present?'),
                        hr(),
                        withMathJax('\\(P(A|S_1\\cup S_2)=\\frac{P((S_1\\cup S_2)|A)\\times P(A)}{P(S_1\\cup S_2)}=\\frac{P(A) \\times [P(S_1|A)+P(S_2|A)-P((S_1 \\cap S_2)|A)]}{P(S_1)+P(S_2)-P(S_1 \\cap S_2)}\\)'),
                        h3('Which is the probability of a disease given that symptom 1 and symptom 2 are both present?'),
                        hr(),
                        h4(withMathJax('\\(P(A|S_1\\cap S_2)=\\frac{P((S_1\\cap S_2)|A)\\times P(A)}{P(S_1\\cap S_2)}\\)')),
                        hr(),
                        fluidPage(
                            column(4,
                                   numericInput('ppA','Probability a priori of the disease',value=0.4,min=0,max=1,step=0.01),
                                   hr(),
                                    numericInput('pS1','Probability of symptom 1 in the population',value=0.4,min=0,max=1,step=0.01),
                                    numericInput('pS1A','Probability of symptom 1 in the disease',value=0.4,min=0,max=1,step=0.01),
                                    numericInput('pS2','Probability of symptom 2 in the population',value=0.4,min=0,max=1,step=0.01),
                                    numericInput('pS2A','Probability of symptom 2 in the disease',value=0.4,min=0,max=1,step=0.01),
                                    numericInput('pS1S2','Probability of symptom 1 and symptom 2 togheter in the population',value=0.4,min=0,max=1,step=0.01),
                                    numericInput('pS1S2A','Probability of symptom 1 and symptom 2 togheter in the disease',value=0.4,min=0,max=1,step=0.01),
                            ) ,
                            column(2),
                            column(4,
                                   hr(),
                                   h4('Probability of having the disease if S1 or S2 are present:'),
                                   h4(withMathJax('\\(P(A|S_1\\cup S_2)\\)')),
                                   h4(textOutput('PAunion')),
                                   hr(),h4('Probability of having the disease if both S1 and S2 are present:'),
                                   h4(withMathJax('\\(P(A|S_1\\cap S_2)\\)')),
                                   h4(textOutput('PAinterseccio')))
                        )
                            
                ),
               tabPanel('Exercises',
                        hr(),
                        actionButton('goP','New problem',icon('play-circle'),style="background-color:lightblue"),
                        br(),br(),
                        uiOutput('problem')
                        
                        
                        
               ) 
               
               ),
    hr(),
    HTML('(c) Albert Sorribas, Ester Vilaprinyo, Rui Alves, Montserrat Rue <br>
         Biomodels Group <br>
         University of Lleida - Institute of Biomedical Research (IRBLleida)')
    
    
    )
    )
