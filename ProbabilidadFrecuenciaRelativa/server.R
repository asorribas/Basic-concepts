#
# Probabilidad y frecuencia relativa
#

library(shiny)
#library(epitools)
library(tidyverse)

# Functions

mostra <- function(n,p) {
  rbinom(n,1,p)
}

MyTheme <- theme(
  axis.title.x = element_text(size = 18),
  axis.text.x = element_text(size = 16),
  axis.text.y = element_text(size = 16),
  axis.title.y = element_text(size = 18),
  axis.line = element_line(colour = "darkblue", 
                           size = 1, linetype = "solid"),
  panel.border = element_rect(colour = "black", fill=NA, size=2))

# Define server logic 
shinyServer(function(input, output) { 
  r <- eventReactive(input$go,{mostra(input$n,input$prob)},ignoreNULL = FALSE)
  t <- eventReactive(input$go,{('Relative frequency obtained with the indicated number of 
                                observations and 95% prediction interval for the expected result.')},ignoreNULL=FALSE)
  r2 <- eventReactive(input$go2,{rbinom(input$ns,input$n2,input$prob2)},ignoreNULL = FALSE)
  
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
  } )
  
  
  
  
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
  
  output$plot2 <- renderPlot({
    p <- input$prob2
    n <- input$n2
    d <- data.frame(x=r2()/n)
    inc <- 1.96*sqrt(p*(1-p)/n)
    
    ggplot(d,aes(x=x))+
      geom_histogram(binwidth = input$binw,col='black',fill='yellow')+
      scale_x_continuous(limits=c(input$xmin,input$xmax))+
      xlab('Relative Frequency')+
      geom_vline(xintercept=c(p+inc,p-inc),color='red',size=1,linetype='dashed') +
      geom_vline(xintercept = p,color='black',size=1,linetype='dashed')+
      ggtitle(paste('Distribution of relative frequency'))+
      theme(plot.title = element_text(hjust = 0.5,size=22))+
      theme(axis.title = element_text(hjust = 0.5,size=22))+
      ylab('Number of samples')+
      xlab('Observed proportion')
    
    
    
  })
  
  output$tablepar <- renderTable({
    p    <- input$prob2
    n    <- input$n2
    data <- r2()
    d    <- data.frame(x=data/n)
    inc  <- 1.96*sqrt(p*(1-p)/n)
    low  <- p-inc
    up   <- p+inc
    dl   <- d %>% filter(x<low)
    du   <- d %>% filter(x>up)
    insi <- input$ns-length(rbind(dl,du)[,])
    data.frame(n=n,p=p,low=low,upper=up)
  })
  
  output$tableres <- renderTable({
    p    <- input$prob2
    n    <- input$n2
    data <- r2()
    d    <- data.frame(x=data/n)
    inc  <- 1.96*sqrt(p*(1-p)/n)
    low  <- p-inc
    up   <- p+inc
    dl   <- d %>% filter(x<low)
    du   <- d %>% filter(x>up)
    insi <- input$ns-length(rbind(dl,du)[,])
    data.frame(samples=input$ns,inside=insi,prop.inside=round(insi/input$ns,3))
  })
  
  output$dBinomial <- renderPlot({
    d <- data.frame(x=factor(0:input$nB),p0=dbinom(0:input$nB,size=input$nB,prob=input$piB))
    ggplot(d,aes(x=x,y=p0))+
      
      geom_bar(stat="identity",width=.05,color='red',fill='red')+
      geom_point(size=5,color='red')+
      ylab('P(X=x)')+
      MyTheme+theme(axis.text.x = element_text(angle = 90,size=10))
    
  })
  
  output$dBinomialFreq <- renderPlot({
    d <- data.frame(x=0:input$nB,n=input$nB,prob=dbinom(0:input$nB,size=input$nB,prob=input$piB))
    d2 <- d %>% mutate(p0=factor(round(x/n,2)))
    
    ggplot(d2,aes(x=p0,y=prob))+
      geom_bar(stat="identity",width=.05,color='red',fill='red')+
      geom_point(size=5,color='red')+
      ylab('Probability')+xlab('Relative frequency in the sample')+
      MyTheme+theme(axis.text.x = element_text(angle = 90,size=10))
    
  })
  
  output$dBinomialTable <- renderTable({
    d <- data.frame(x=0:input$nB,n=input$nB,prob=dbinom(0:input$nB,size=input$nB,prob=input$piB))
    d2 <- d %>% mutate(p0=x/n)
    d2 <- d2[,c(1,4,3)]
    d2 %>% dplyr::filter(prob>0.01)
    
  })
  
  
  })
  
 