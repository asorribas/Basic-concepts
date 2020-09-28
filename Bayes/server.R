#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)



# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    p1A <- eventReactive(input$goP,{runif(1,0.1,0.9)},ignoreNULL=FALSE)
    p1SA <- eventReactive(input$goP,{runif(1,0.5,0.9)},ignoreNULL=FALSE)
    p1SAc <- eventReactive(input$goP,{runif(1,0.1,0.4)},ignoreNULL=FALSE)
    
    p2A <- eventReactive(input$go2,{runif(1,0.1,0.9)},ignoreNULL=FALSE)
    p2SA <- eventReactive(input$go2,{runif(1,0.5,0.9)},ignoreNULL=FALSE)
    p2SAc <- eventReactive(input$go2,{runif(1,0.1,0.4)},ignoreNULL=FALSE)
    
    
    p3A1 <- eventReactive(input$goP,{runif(1,0.1,0.3)},ignoreNULL=FALSE)
    p3A2 <- eventReactive(input$goP,{runif(1,0.3,0.6)},ignoreNULL=FALSE)
    p3A3 <- eventReactive(input$goP,{1-p3A1()-p3A2()},ignoreNULL=FALSE)
    
    p3SA1 <- eventReactive(input$goP,{runif(1,0.1,0.5)},ignoreNULL=FALSE)
    p3SA2 <- eventReactive(input$goP,{runif(1,0.1,0.9)},ignoreNULL=FALSE)
    p3SA3 <- eventReactive(input$goP,{runif(1,0.1,0.9)},ignoreNULL=FALSE)
    
    output$problem <- renderUI({
        
        pA <-round(p1A(),2)
        pSA<-round(p1SA(),2)
        pSAc <- round(p1SAc(),2)
        
        
        text1 <- HTML(paste('The people that suffers a given disease has a probability of',pSA,'
                      of presenting a given symptom. For healthy people, this probability
                      is',pSAc,'. If the prevalence of that disease in the population is',
                            pA,' which is the probability that a person that shows the symptom
                      has the disease? <br>
                      <b>Solution:<b/>',round(pSA*pA/(pSA*pA+pSAc*(1-pA)),2)))
        
        pA<-round(p2A(),3)
        pSA<-round(p2SA(),3)
        pSAc <- round(p2SAc(),3)
        text2 <- HTML(paste('A test gives a positive result (+) for people infected in a',pSA*100,'
                      % of the cases. For healthy people, this probability
                      is',pSAc*100,'%. If',
                           pA*100,'% of the people is infected, which is the proportion of persons 
                      that shows a positive that will be actually infected?<br>
                      <b>Solution:<b/>',round(pSA*pA/(pSA*pA+pSAc*(1-pA))*100,1),'%'))
        
        
        pA1<-round(p3A1(),2)
        pA2<-round(p3A2(),2)
        pA3<-round(p3A3(),2)
        pSA1<-round(p3SA1(),3)
        pSA2<-round(p3SA2(),3)
        pSA3<-round(p3SA3(),3)
        pS <- pSA1*pA1+pSA2*pA2+pSA3*pA3
        p1 <- round(pSA1*pA1/pS,2)
        p2 <- round(pSA2*pA2/pS,2)
        p3 <- round(pSA3*pA3/pS,2)
        text3 <- HTML(paste('In an infected subject, we suspect of three different pathogens. Based on previous
                        tests, we consider that the probability of being infected by each one of the
                        pathogens is',pA1,',',pA2,',',pA3,'. Vomits are oberved in', pSA1*100, '% of the people
                        infected by the first pathogen, in',pSA2*100,'% of the people infected by the second, and in the',pSA3*100,'%
                        of those infected by the third one. If a person shows vomits, which is ther probability that a person is
                        infected by each pathogen?<br>
                      <b>Solution:<b/>',p1,',',p2,',',p3))
        
        pA <-round(p1A(),2)
        sen<-round(p1SA(),2)
        espec <- round(1-p1SAc(),2)
        text4 <- HTML(paste('The sensitivity of a test is',sen,' and the specificity 
                      is',espec,'. If we apply the test to a population where the prevalence of the disease is',
                            pA,' which is the probability that a person that has a positive result is not affected by the disease?
                       <br>
                      <b>Solution:<b/>',round((1-espec)*(1-pA)/((1-espec)*(1-pA)+sen*pA),2)))
        
        
        pA <-round(p1A(),2)
        pSA<-round(p1SA(),2)
        pSAc <- round(p1SAc(),2)
        text5 <- HTML(paste('The sensitivity of a test is',pSA,'. Which percentage of false negative results will be observed if the 
            prevalence of the disease is, ',pA,'?
                       <br>
                      <b>Solution:<b/>',round(pA*(1-pSA)*100,2),'%'))
        
        pA <-round(p1A(),2)
        pSA<-round(p1SA(),2)
        pSAc <- round(p1SAc(),2)
        text6 <- HTML(paste('The especificity of a test is',pSAc,'. Which percentage of false positive results will be observed if the 
            prevalence of the disease is, ',pA,'?
                       <br>
                      <b>Solution:<b/>',round((1-pA)*(1-pSAc)*100,2),'%'))
        
        pA <-round(p1A(),2)
        pSA<-round(p1SA(),2)
        pSAc <- round(p1SAc(),2)
        FN <- round((1-pSA)*pA,2)
        text7 <- HTML(paste('If a disease affects an',pA*100,'% of the population, which sensitivity is required for
                        a test to assure a maximum of a ', FN*100,'% of false negative results?
                       <br>
                      <b>Solution:<b/>',round(pSA,2)))
        
        
        sel <- sample(1:7,1)
        if (sel==1) {text<-text1}
            else if(sel==2) {text<-text2}
                else if (sel==3) {text<-text3}
                        else if (sel==4) {text<-text4}
                                else if (sel==5) {text<-text5}
                                    else if (sel==6) {text<-text6}
                                else {text<-text7}
            
        
    })
    
   

    output$PB <- renderText({pB=input$pBA1*input$pA1+input$pBA2*input$pA2+input$pBA3*input$pA3+input$pBA4*input$pA4})
    
    
    output$posterior <- renderPlot({
        pB=input$pBA1*input$pA1+input$pBA2*input$pA2+input$pBA3*input$pA3+input$pBA4*input$pA4
        pA1B=input$pBA1*input$pA1/pB
        pA2B=input$pBA2*input$pA2/pB
        pA3B=input$pBA3*input$pA3/pB
        pA4B=input$pBA4*input$pA4/pB
        nn=c('A1','A2','A3','A4')
        priori=c(input$pA1,input$pA2,input$pA3,input$pA4)
        posteriori=c(pA1B,pA2B,pA3B,pA4B)
        d <- data.frame(disease=c(nn,nn),Diagnostic=c(rep('a priori',4),rep('a posteriori',4)),prob=c(priori,posteriori))
        ggplot(d,aes(x=disease,fill=Diagnostic))+geom_bar(aes(weight=prob),position=position_dodge())+ylim(0,1)
    })

    output$posteriorTable <- renderTable({
        pB=input$pBA1*input$pA1+input$pBA2*input$pA2+input$pBA3*input$pA3+input$pBA4*input$pA4
        pA1B=input$pBA1*input$pA1/pB
        pA2B=input$pBA2*input$pA2/pB
        pA3B=input$pBA3*input$pA3/pB
        pA4B=input$pBA4*input$pA4/pB
        nn=c('A1','A2','A3','A4','Sum')
        priori=c(input$pA1,input$pA2,input$pA3,input$pA4)
        priori=c(priori,sum(priori))
        posteriori=c(pA1B,pA2B,pA3B,pA4B)
        posteriori=c(posteriori,sum(posteriori))
        d <- data.frame(disease=nn,priori,posteriori)
        d
    })
    
    output$PAunion <- renderText({
        r=input$ppA*(input$pS1A+input$pS2A-input$pS1S2A)/(input$pS1+input$pS2-input$pS1S2)
        round(r,3)
    })
    
    output$PAinterseccio<- renderText({
        r=input$ppA*(input$pS1S2A)/(input$pS1S2)
        round(r,3)
    })
    
    output$Bayes <- renderPlot({
        PA <- input$pA
        PSA <- input$pSA
        PSAc <- input$pSAc
       
        fun <- function(psa,psac,pa) psa*pa/(psa*pa+psac*(1-pa))
        p0 <- fun(PSA,PSAc,PA)
        prob <- seq(0,1,.01)
        d <- data.frame(prob)
        d <- d %>% mutate(p=fun(PSA,PSAc,prob))
        ggplot(d,aes(x=prob,y=p))+
            geom_line()+
            xlab('P(A)')+
            ylab('P(A|S)')+
            geom_segment(y = p0, x=0,yend=p0,xend=PA,linetype='dashed' )+
            geom_segment(y = 0, x=PA,yend=p0,xend=PA,linetype='dashed' )+
            annotate('point',x=PA,y=p0,colour='blue',size=3)+
            theme_bw()
        
        
    })
    
    output$BayesPrior <- renderText({
        p<-input$pA
        round(p,3)})
    
    output$BayesPosterior <- renderText({
        PA <- input$pA
        PSA <- input$pSA
        PSAc <- input$pSAc
        
        fun <- function(psa,psac,pa) psa*pa/(psa*pa+psac*(1-pa))
        p0 <- fun(PSA,PSAc,PA)
        round(p0,3)
    })
    
    
    output$FPR <- renderText({
        p <- round((1-input$Prev)*(1-input$Espec),2)
        text <- paste('False positive rate:',p)
        text
    })
    
    output$FNR <- renderText({
        p <- round((input$Prev)*(1-input$Sensib),2)
        text <- paste('False positive rate:',p)
        text
    })
    
    output$PDE <- renderText({
        p1 <- round((input$Prev)*(1-input$Sensib),2)
        p2 <- round((1-input$Prev)*(1-input$Espec),2)
        text <- paste('Probability of an erroneous diagnostic:',p1+p2)
        text
    })
    
    dataTableTest <- eventReactive(input$goTest,{
        p <- input$Prev
        n <- input$nTest
        x <-sample(0:1,n,prob=c(1-p,p),replace = TRUE)
        d <- data.frame(Status=factor(x,labels=c('Healthy','Disease')))
        t <- c()
        for (i in 1:n)
            if (x[i]==0) {t[i]<- sample(0:1,1,prob=c(input$Espec,1-input$Espec))}
        else {t[i]<- sample(0:1,1,prob=c(1-input$Sensib,input$Sensib))}
        d <- d %>% mutate(test=factor(t,labels=c('(-)','(+)')))
        d
    },ignoreNULL = FALSE)
    
    output$TableTest <- renderPrint({
        t <- dataTableTest() %>% table()
        t2 <- dataTableTest() %>% dplyr::select(Status) %>% table()
        
        list(Status=prop.table(t2),
             'Table of results'=t,'Table of FPR, FNR, etc'=round(prop.table(t),2),
             'Sensitivity and Especificity'=round(prop.table(t,1),2))
    })
    
    output$PPV <- renderText({
        FN <- round((input$Prev)*(1-input$Sensib),2)
        FP <- round((1-input$Prev)*(1-input$Espec),2)
        VP <- round((input$Prev)*(input$Sensib),2)
        VN <- round((1-input$Prev)*(input$Espec),2)
        r <- round(VP/(FP+VP),2)
        text <- paste('PPV:',r)
    })
    
    output$NPV <- renderText({
        FN <- round((input$Prev)*(1-input$Sensib),2)
        FP <- round((1-input$Prev)*(1-input$Espec),2)
        VP <- round((input$Prev)*(input$Sensib),2)
        VN <- round((1-input$Prev)*(input$Espec),2)
        r <- round(VN/(FN+VN),2)
        text <- paste('NPV:',r)
        text
    })
    
    
})
