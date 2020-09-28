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
library(readxl)
library(dplyr)


####################################################
### FUNCTIONS
###################################################

boot.sample <- function(x){
    n <- length(x)
    s <- sample(x,n,replace=T)
    s
}

boot.Quantile <- function(x,conf=0.95){
    s <- boot.sample(x)
    quantile(s,c((1-conf)/2,1-(1-conf)/2),type=6)
}

boot.Normality.Interval <- function(x,conf=0.95) {
    m <- replicate(10000,boot.Quantile(x))
    
    m
}



Compare.Normality.Intervals <- function(x,conf=0.95) { 
    
    n <- length(x) 
    t <- qt(1-(1-conf)/2,n-1)
    me <- mean(x)
    se <- sd(x)
    
    inc.t <- t*sqrt((n+1)/n)*se
    
    in.1 <- c('Student',round(c(me-inc.t,me+inc.t),2))
    in.2 <- c('Sample quantiles',
              round(quantile(x,c((1-conf)/2,1-(1-conf)/2),type=6),2))
    
    s <- boot.Normality.Interval(x)
    in.3 <- c('Bootstrap',round(c(mean(s[1,]),mean(s[2,])),2))
    
    d <- rbind(in.1,in.2,in.3)
    colnames(d)<-c('Method','2.5%','97.5%')
    d
}

RI.Normal.Sample <-function(x,conf=0.95) {
    m <- mean(x)
    s <- sd(x)
    n <- length(x)
    t <- qt(1-(1-conf)/2,n-1)
    n <- length(x) 
    inc <- t * s * sqrt((n+1)/n)
    
    c(m-inc,m+inc)
}

RI.Normal.Estimated <- function(n,m,s,conf=0.95) {
    
    t <- qt(1-(1-conf)/2,n-1)
    
    inc <- t * s * sqrt((n+1)/n)
    
    c(m-inc,m+inc)
}



CI.RI.Normal.Sample <- function(x,conf=0.95) {
    
    ri <- RI.Normal.Sample(x)
    lower <- ri[1]
    upper <- ri[2]
    n <- length(x)
    
    reft<- qt(1-(1-conf)/2,n-1)
    se <- sqrt(((sd(x)^2)/n) + (((reft^2) * (sd(x)^2))/(2 * n)))
    limitt <- qt(1-(1-conf)/2,n-1)
    
    list(lower=c(ri[1] - limitt* se,ri[1],ri[1] + limitt* se),
         upper=c(ri[2] - limitt* se,ri[2],ri[2] + limitt* se))
}

CI.RI.Normal.Estimated <- function(n,m,s,conf=0.95) {
    
    ri <- RI.Normal.Estimated(n,m,s)
    lower <- ri[1]
    upper <- ri[2]
    
    
    reft<- qt(1-(1-conf)/2,n-1)
    se <- sqrt(((s^2)/n) + (((reft^2) * (s^2))/(2 * n)))
    limitt <- qt(1-(1-conf)/2,n-1)
    
    list(lower=c(ri[1] - limitt* se,ri[1],ri[1] + limitt* se),
         upper=c(ri[2] - limitt* se,ri[2],ri[2] + limitt* se))
}


############################################
# Define server logic 
############################################

shinyServer(function(input, output) {
    
    sample <- eventReactive(input$goCompare, {rnorm(input$n,input$m,input$s)},ignoreNULL = FALSE)
    
    r <- eventReactive(input$goCompare,{Compare.Normality.Intervals(sample(),0.95)},ignoreNULL = FALSE)
    
    Ref.Int <- eventReactive(input$goRefInt,{CI.RI.Normal.Estimated(input$obsn,input$obsmu,input$obssd)})
    
    data <- eventReactive(input$go, {
      infile<-input$file
      x <- read_excel(input$file$datapath,1)
      return(x)
    })
    
    dsel <- eventReactive(input$vars,{
     data() %>% dplyr::select(input$vars)
    })
      
     
    output$summary <- renderPrint({
        summary(sample())
    })
        
        
        output$Compare <- renderTable({
        r()
    })
    
    output$Original1 <- output$Original2 <- renderTable({
        m <- input$m
        s <- input$s
        z <- qnorm(1-(1-0.95)/2)
        t <- data.frame('Reference interval',low=round(c(m-z*s),2),upper=round(c(m+z*s),2))
        colnames(t)<-c('','2.5%','97.5%')
        t
    })
    
    output$Histo <- renderPlot({
        d <- sample()
        m <- input$m
        s <- input$s
        dd.x <- data.frame(Biomarker=seq(m-3*s,m+3*s,.1))
        dd <- data.frame(Biomarker=d)
        p <- ggplot(dd,aes(x=Biomarker))+
            geom_histogram(aes(y=..density..),color='black',fill='yellow',binwidth = input$bw)
        
        if  (input$Densi.Normal==FALSE & input$Densi.Estimated==FALSE){
            pp<-p
        }
        else if (input$Densi.Normal==TRUE & input$Densi.Estimated==FALSE) {
            pp<-p+stat_function(data=dd.x,fun=dnorm,
                            color="red",
                            args=list(mean=input$m, 
                                      sd=input$s),
                            size=1)}
        else if (input$Densi.Normal==TRUE & input$Densi.Estimated==TRUE) {
            pp<-p+stat_function(data=dd.x,fun=dnorm,
                                color="red",
                                args=list(mean=input$m, 
                                          sd=input$s),
                                size=1)+
                stat_function(data=dd.x,fun=dnorm,
                              color="red",
                              args=list(mean=mean(d), 
                                        sd=sd(d)),
                              size=1,linetype='dashed')} 
        else if (input$Densi.Normal==FALSE & input$Densi.Estimated==TRUE) {
            pp<-p+stat_function(
                data=dd.x,
                fun=dnorm,
                              color="red",
                              args=list(mean=mean(d), 
                                        sd=sd(d)),
                                        size=1,linetype='dashed')}
            
        pp
    })
    
    output$RefLim <- renderTable({
        x <- sample()
        rbind(
            refLimit(x,RI='p',limitConf = 0.95)$Ref_Int,
            refLimit(x,RI='n',limitConf = 0.95)$Ref_Int,
            refLimit(x,RI='r',limitConf = 0.95)$Ref_Int)
    })
    
    output$Formulas <- renderUI({
        
        if (input$optConcept==1) {
            withMathJax('A reference interval is a range of values (a,b) so that:
                        $$P(a \\le X \\le b)=1-\\alpha$$
                        For example, a 95% reference interval (a,b) is obtained so that:
                        $$P(a \\le X \\le b)=0.95$$
                        A 95% reference internal (also known as normality interval or as a reference range) is
                        an interval that predicts the values of the 95% of subjects of that population.
                        In the case of clinical reference ranges, those are the values that are use as
                        a warning of possible health problems.')
        }
        else if(input$optConcept==2){
            withMathJax('For a normal distribution $$N\\mu,\\sigma)$$ the reference interval (a,b) so that:
            $$P(a \\le X \\le b)=1-\\alpha$$
            is:
            $$\\mu \\pm z_{1-\\alpha/2} \\times \\sigma.$$
            In the particular case of a 95% reference interval:
                        $$\\mu \\pm 1.96 \\times \\sigma \\approx \\mu \\pm 2 \\times \\sigma$$ 
            This is the range of values where we expect to observe the 95% of the values in a sample.')}
        else if(input$optConcept==3){
            withMathJax('Given a sample of values of a biomarker (in g/L, U/L, etc.) we calculate its mean (m) 
            and estandard deviation (sd). With that, the reference interval is computed as:
                    $$m \\pm t_{1-\\alpha/2,n-1} \\times sd \\times \\sqrt{\\frac{(n+1)}{n}}$$
            This is valid if the sample comes from a normal distributed variable.')}
         else if(input$optConcept==4) {
            withMathJax('For large samples, and especially when the distribution is non-normal, we can
            compute the reference interval using the appropriate quantiles. That is:
                     $$(a,b) \\to (Q_{\\alpha/2},Q_{1-\\alpha/2})$$')}
          else if(input$optConcept==5) {
            HTML('A bootstrap reference interval is obtained as follows: <br/>
                        (1) Generate a boostrap sample of the data  <br/>
                        (2) Compute the reference interval for the boostrap sample using quantiles  <br/>
                        (3) Repeat (1) and (2) many times (for instance 5000)  <br/>
                        (4) Compute the mean of the obtained quantiles.')}
        })
      
    output$RefInt <- renderTable({
        rr <- Ref.Int()
        r<-rbind(c('Lower','Upper'),
            round(c(rr$lower[2], rr$upper[2] ),2))
        
    },include.colnames=FALSE)   
    
    output$RefInt.CI <- renderTable({
        rr <- Ref.Int()
        rbind(c('','2.5%','97.5%'),
               c('lower', round(rr$lower[1],2),round(rr$lower[3],2)),
               c('upper',round(rr$upper[1],2),round(rr$upper[3],2))
        )
    },include.colnames=FALSE) 
    
    output$PlotRefInt <- renderPlot({
        rr <- Ref.Int()
        m <- input$obsmu
        s <- input$obssd
        d <- data.frame(id='Ref.Int',ir=c(rr$low[2],rr$upper[2]),lower=c(rr$low[1],rr$upper[1]),upper=c(rr$low[3],rr$upper[3]))
        d
        if (input$CIYN==TRUE){ 
        ggplot( )+
            geom_errorbar(data=d, mapping=aes(x=id, ymin=upper, ymax=lower), width=0.2, size=1, color="blue") + 
                        geom_segment(aes(x=d$id[1],y=d$ir[1],
                                         xend=d$id[1],yend=d$ir[2]),
                                         color='red',size=2)+
                        geom_point(data=d, mapping=aes(x=id,y=ir), size=4, shape=21, fill="white")+
                        ylim(input$xminRI,input$xmaxRI)+
                        coord_flip()+
                        xlab('') + 
                        ylab('Biomarker') }
        else if (input$CIYN==FALSE){
          ggplot( )+
             geom_segment(aes(x=d$id[1],y=d$ir[1],
                             xend=d$id[1],yend=d$ir[2]),
                         color='red',size=2)+
            geom_point(data=d, mapping=aes(x=id,y=ir), size=4, shape=21, fill="white")+
            ylim(input$xminRI,input$xmaxRI)+
            coord_flip()+
            xlab('') + 
            ylab('Biomarker') 
        }
            
        
    })
    
    ###################################
    # Read external excel files
    output$varChoice <- renderUI({
      selectInput('vars','Select variables to show:',
                  choices=as.list(names(data())),
                  selected=NULL,multiple = FALSE)
      
    })
    output$text1 <-renderTable({
      head(data())})
    
    output$text3 <- renderText({
      names(data())
    })
    output$text2 <-renderTable({
      d <-data()
      d %>% dplyr::select(input$vars) %>% head()})
    
    output$resumVars <- renderPrint({
      d <-data()
      d %>%summarise_all(typeof) %>% gather
    })
    
    output$FileRI <- renderTable({
      d <-as.data.frame(dsel())
      Compare.Normality.Intervals(d[,1]) 
    })
    
    output$FileCI <- renderTable({
      d <-as.data.frame(dsel())
       
      rr <- CI.RI.Normal.Sample(d[,1])
      
      rbind(c('','2.5%','97.5%'),
            c('lower', round(rr$lower[1],2),round(rr$lower[3],2)),
            c('upper',round(rr$upper[1],2),round(rr$upper[3],2))
      )
    },colnames = FALSE)
    
    
    output$FilePlot <- renderPlot({
      d <-as.data.frame(dsel())
      d <- data.frame(x=d[,1])
      if (input$TypePlot==1) {
      ggplot(d,aes(x=x))+
        geom_histogram(aes(y = ..density..),color='black',fill='yellow',binwidth=input$bwFile)+
        geom_density(color='red',size=1)
        }
      else if (input$TypePlot==2){
        ggplot(d,aes(sample=x))+
          geom_qq()+geom_qq_line()
      }
    })
    
    output$VarSelected <- renderText({
      paste('Selected variable:',input$vars)
    })
                   
})

    

   


