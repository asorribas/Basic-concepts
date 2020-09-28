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
library(ggpubr)

shade_curve <- function(MyDF, xstart, xend, fill = "red", alpha = .5){
  geom_area(data = subset(MyDF, x >= xstart
                          & x < xend),
            aes(y=y), fill = fill, color = NA, alpha = alpha)
}

plot.normal.ab <- function(mu,sigma,a,b,opt) {
  x <- seq(mu-4*sigma,mu+4*sigma, 0.05)
  MyDF <- data.frame(x = x, y = dnorm(x, mean =mu, sd = sigma))
  p <- pnorm(b,mu,sigma)-pnorm(a,mu,sigma)
  
  p1 <- ggplot(MyDF, aes(x = x, y = y)) + 
    geom_line() +
    geom_point(aes(x=mu,y=0,size=2))+
    theme(legend.position = 'none')
  
  if (opt==1) {
    p1 +
    shade_curve(MyDF = MyDF, xstart = a, xend = b, fill = 'blue', alpha = .3)+
    annotate(geom="text", x=(b+a)/2, y=round(dnorm(mu,mu,sigma)*1.2,3), label=round(p,3),
             color="blue",size=10)+
      ylim(0,0.1)+
      xlim(60,140)+
      ylab('Density')+
      geom_hline(yintercept = 0)}
  else if (opt==2){
    p1 +
      shade_curve(MyDF = MyDF, xstart = 0, xend = a, fill = 'blue', alpha = .3)+
      annotate(geom="text", x=(b+a)/2, y=round(dnorm(mu,mu,sigma)*1.2,3), label=round(pnorm(a,mu,sigma),3),
               color="blue",size=10)+
      ylim(0,0.1)+
      xlim(60,140)+
      ylab('Density')+
      geom_hline(yintercept = 0)}
  else if (opt==3){
    p1 +
      shade_curve(MyDF = MyDF, xstart = 0, xend = b, fill = 'blue', alpha = .3)+
      annotate(geom="text", x=(b+a)/2, y=round(dnorm(mu,mu,sigma)*1.2,3), label=round(pnorm(b,mu,sigma),3),
               color="blue",size=10)+
      ylim(0,0.1)+
      xlim(60,140)+
      ylab('Density')+
      geom_hline(yintercept = 0)}
  else if (opt==4){
    p1 +
      shade_curve(MyDF = MyDF, xstart = a, xend = 1000, fill = 'blue', alpha = .3)+
      annotate(geom="text", x=(b+a)/2, y=round(dnorm(mu,mu,sigma)*1.2,3), label=round(1-pnorm(a,mu,sigma),3),
               color="blue",size=10)+
      ylim(0,0.1)+
      xlim(60,140)+
      ylab('Density')+
      geom_hline(yintercept = 0)}
  else if (opt==5){
    p1 +
      shade_curve(MyDF = MyDF, xstart = b, xend = 1000, fill = 'blue', alpha = .3)+
      annotate(geom="text", x=(b+a)/2, y=round(dnorm(mu,mu,sigma)*1.2,3), label=round(1-pnorm(b,mu,sigma),3),
               color="blue",size=10)+
      ylim(0,0.1)+
      xlim(60,140)+
      ylab('Density')+
      geom_hline(yintercept = 0)}
  else if (opt==6){
    p1 +
      shade_curve(MyDF = MyDF, xstart = 0, xend = mu, fill = 'blue', alpha = .3)+
      annotate(geom="text", x=(b+a)/2, y=round(dnorm(mu,mu,sigma)*1.2,3), label=round(1-pnorm(mu,mu,sigma),3),
               color="blue",size=10)+
      ylim(0,0.1)+
      xlim(60,140)+
      ylab('Density')+
      geom_hline(yintercept = 0)}
      
  
}

plot.sigma.interval <- function(mu,sigma,k,xRange,yRange) {
  
  
  x <- seq(mu-4*sigma,mu+4*sigma, 0.05)
  a <- mu-k*sigma
  b <- mu+k*sigma
  
  MyDF <- data.frame(x = x, y = dnorm(x, mu, sigma))
  p <- pnorm(b,mu,sigma)-pnorm(a,mu,sigma)
  
  ggplot(MyDF, aes(x = x, y = y)) + 
    geom_line() +
    shade_curve(MyDF = MyDF, xstart = a, xend = b, fill = 'blue', alpha = .3)+
    annotate(geom="text", x=mu, y=dnorm(mu,mu,sigma)*1.1, label=round(p,3),
             color="blue",size=12)+
    ylim(0,yRange)+
    xlim(xRange[1],xRange[2])+
    ylab('Density')+
    geom_hline(yintercept = 0)+
    geom_point(aes(x=mu,y=0,size=2))+
    theme(legend.position = 'none')+
    geom_segment(x=mu,y=0,xend=mu,yend=dnorm(mu,mu,sigma),
                 linetype='dashed')+
    geom_segment(x=mu+k*sigma,y=dnorm(mu+k*sigma,mu,sigma),xend=mu,yend=dnorm(mu+k*sigma,mu,sigma),
                 linetype='dashed')+
    geom_segment(x=mu,y=0,xend=mu+k*sigma,yend=0,size=2,
                 arrow=arrow(length=unit(0.5,'cm')))+
    geom_point(aes(x=mu,y=0,size=2))+
    theme(legend.position = 'none')
}

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
   data <- eventReactive(input$go,{data.frame(x=rnorm(input$n,input$mu,input$sigma))},ignoreNULL = FALSE)
    
   output$plot.ab <- renderPlot({
     plot.normal.ab(input$mu,input$sigma,input$a,input$b,input$optAreas)
      
   })
   
   output$histogram <- renderPlot({
     #x <- seq(input$mu-3*input$sigma,input$mu-3*input$sigma,0.01)
     s <- data()
     mest <- mean(s$x)
     sdest <- sd(s$x)
     m <- input$mu
     sd <- input$sigma
     binwidth <- input$bin
     if((input$dcurve==TRUE) & (input$estcurve==TRUE) )
     {ggplot(s, aes(x = x, mean = m, sd = sd, binwidth = binwidth, n = input$n)) +
         theme_bw() +
         geom_histogram(binwidth = binwidth, 
                        colour = "black", fill = "yellow", size = 0.1) +
         stat_function(fun = function(x) dnorm(x, mean = m, sd = sd) * input$n * binwidth,
                       color = "darkred", size = 1)+
         stat_function(fun = function(x) dnorm(x, mean = mest, sd = sdest) * input$n * binwidth,
                       color = "darkred", size = 1,linetype='dashed')+
         xlim(60,140)
         }
     else if ((input$dcurve==FALSE) & (input$estcurve==FALSE) )
         {ggplot(s, aes(x = x, mean = m, sd = sd, binwidth = binwidth, n = input$n)) +
         theme_bw() +
         geom_histogram(binwidth = binwidth, 
                        colour = "black", fill = "yellow", size = 0.1)+
         xlim(60,140)}
     else if ((input$dcurve==TRUE) & (input$estcurve==FALSE) )
     {ggplot(s, aes(x = x, mean = m, sd = sd, binwidth = binwidth, n = input$n)) +
         theme_bw() +
         geom_histogram(binwidth = binwidth, 
                        colour = "black", fill = "yellow", size = 0.1) +
         stat_function(fun = function(x) dnorm(x, mean = m, sd = sd) * input$n * binwidth,
                       color = "darkred", size = 1)+
         xlim(60,140)}
     else
     {
       ggplot(s, aes(x = x, mean = m, sd = sd, binwidth = binwidth, n = input$n)) +
         theme_bw() +
         geom_histogram(binwidth = binwidth, 
                        colour = "black", fill = "yellow", size = 0.1) +
         stat_function(fun = function(x) dnorm(x, mean = mest, sd = sdest) * input$n * binwidth,
                       color = "darkred", size = 1,linetype='dashed')+
         xlim(60,140)
     }
   })
   
   output$plot.normal <- renderPlot({
     k <- switch(input$kopt,
                '1'=1,
                '2'=1.28,
                '3'=1.96,
                '4'=2,
                '5'=3)
     plot.sigma.interval(input$mu,input$sigma,k,input$xRange,input$yRange)
   })
   
   output$boxplot <- renderPlot({
     d <- data()
     if (input$points==TRUE) {
       ggplot(d,aes(x=factor('Sample'),y=x))+
       geom_boxplot(width=0.5,outlier.colour='red',outlier.size = 4)+
       geom_dotplot(binaxis = "y",stackdir = 'center',size=1,alpha=0.3,color='blue')+
       geom_point(aes(y=mean(d$x)),size=4,color='green',alpha=0.9)+
       coord_flip()+
       xlab('')+ylim(60,140)}
     else {
       ggplot(d,aes(x=factor('Sample'),y=x))+
         geom_boxplot(width=0.5,outlier.colour='red',outlier.size = 4)+
         geom_point(aes(y=mean(d$x)),size=4,color='green',alpha=0.9)+
         coord_flip()+
         xlab('')+ylim(60,140)
     }
   })
   
  
   
   output$qq <- renderPlot({
     d <- data()
     if (input$qqline==TRUE)
      ggplot(d,aes(sample=x))+geom_qq()+geom_qq_line()
     else
       ggplot(d,aes(sample=x))+geom_qq()
   })
   
   output$NormalityTest <- renderPrint({
     d<-data()
     r<-shapiro.test(d$x)
     r
   })
   
   output$textab <- renderText({
     'pnorm(b,mu,sigma)-pnorm(a,mu,sigma)'
   })
   output$textltx <- renderText({
     'pnorm(x,mu,sigma) )'
   })
   output$textgtx <- renderText({
     '1-pnorm(x,mu,sigma) )'
   })
   
   output$pX <- renderText({
     text <- 'pnorm(100,3,102)'
     text
   })
   
   output$pZ <- renderText({
     text <- 'pnorm(0.67)'
     text
   })
})
