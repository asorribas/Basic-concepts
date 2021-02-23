#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(tidyverse)
#library(gridExtra)
library(ggpubr)
library(xtable)
library(plotROC)


fun <- function(xc,mu.e,sd.e,mu.s,sd.s,xmin,xmax) {
  
  puntos <- data.frame(x=seq(xmin,xmax,0.5))
  ifelse (mu.e >= mu.s,
          pp<-ggplot(puntos,aes(x))+
            stat_function(fun=dnorm,args=list(m=mu.e,sd=sd.e),colour='red',size=1)+
            stat_function(fun=dnorm,geom='area',
                          args=list(m=mu.e,sd=sd.e),colour='red',fill = "pink",size=1,
                          xlim=c(xc,xmax))+
            stat_function(fun=dnorm,args=list(m=mu.s,sd=sd.s),colour='blue',size=1)+
            stat_function(fun=dnorm,geom='area',
                          args=list(m=mu.s,sd=sd.s),colour='red',fill = "blue",alpha=0.5,
                          xlim=c(xmin,xc))+
            stat_function(fun=dnorm,args=list(m=mu.s,sd=sd.s),colour='blue',size=1)+
            theme_bw()
          ,
          pp<-ggplot(puntos,aes(x))+
            stat_function(fun=dnorm,args=list(m=mu.e,sd=sd.e),colour='red',size=1)+
            stat_function(fun=dnorm,geom='area',
                          args=list(m=mu.e,sd=sd.e),colour='red',fill = "pink",size=1,
                          xlim=c(xmin,xc))+
            stat_function(fun=dnorm,args=list(m=mu.s,sd=sd.s),colour='blue',size=1)+
            stat_function(fun=dnorm,geom='area',
                          args=list(m=mu.s,sd=sd.s),colour='red',fill = "blue",alpha=0.5,size=1,
                          xlim=c(xc,xmax))+
            theme_bw()
  )
  pp + ylab('Density') + xlab('Biomarker')+
    geom_hline(yintercept = 0)+
    geom_point(aes(x=mu.s,y=0),size=4,color='blue')+
    geom_point(aes(x=mu.e,y=0),size=4,color='red')+
    geom_point(aes(x=xc,y=0),size=6,color='black',fill='white',shape=24)+
    scale_x_continuous(breaks=seq(xmin,xmax,2))+
    theme(axis.title.x = element_text(face="bold", colour="#990000", size=20),
          axis.text.x  = element_text(angle=0, vjust=0.5, size=16),
          axis.title.y = element_text(face="bold", colour="#990000", size=20),
          axis.text.y  = element_text(angle=0, vjust=0.5, size=16))
    
    
}

sen <- function(x,mu.e,sd.e,mu.s,sd.s) {
  ifelse(mu.e>mu.s,
         1-pnorm(x,mu.e,sd.e),
         pnorm(x,mu.e,sd.e))
}

ROC <- function(me,se,ms,ss,xmin,xmax){
  punts <- seq(xmin,xmax,.05)
  if(me>ms) sen <-1-pnorm(punts,me,se)
  else sen <- pnorm(punts,me,se)
  if(me>ms) esp<-pnorm(punts,ms,ss)
  else esp <- 1-pnorm(punts,ms,ss)
  data.frame(x=punts,sen=sen,esp=esp)
  
}

simple_auc <- function(TPR, FPR){
  # inputs already sorted, best scores first 
  dFPR <- c(diff(FPR), 0)
  dTPR <- c(diff(TPR), 0)
  sum(TPR * dFPR) + sum(dTPR * dFPR)/2
}

Compute.AUC <- function(me,se,ms,ss,xmin,xmax){
  punts <- seq(xmin,xmax,.01)
  if(me>ms) sen <-1-pnorm(punts,me,se)
  else sen <- pnorm(punts,me,se)
  if(me>ms) esp<-pnorm(punts,ms,ss)
  else esp <- 1-pnorm(punts,ms,ss)
  
  round(simple_auc(rev(sen),rev(1-(esp))),3)
}

fPPV <- function(sen,esp,x) x*sen/(sen*x+(1-esp)*(1-x))
fNPV <- function(sen,esp,x)  esp*(1-x)/(esp*(1-x)+(1-sen)*x)

mostraEmpD <- function(nd,md,sdd) {
  
  d <- rnorm(nd,md,sdd)
  r <- data.frame(x=d)
  r
}

mostraEmpH <- function(nh,mh,sdh) {
  
  h <- rnorm(nh,mh,sdh)
  r <- data.frame(x=h)
  r
}

Simula.Un.Biomarker.ROC <- function(n,p,mD,sdD,mH,sdH) {
  
  D.ex <- rbinom(n,1,p)
  
  M1<-sapply(D.ex,function(x){
    if (x==1) { rnorm(1,mD,sdD)}
    else if (x==0) {rnorm(1,mH,sdH)}
  })
  
  test <- data.frame(D=D.ex,D.str=c("Healthy",'Ill')[D.ex+1],
                     M1=M1,stringsAsFactors = FALSE)
  
  p3<-ggplot(test,aes(d=D,m=M1))+
    geom_roc(color='red')+
    style_roc()+
    coord_fixed()+
    geom_segment(x=0,y=0,xend=1,yend=1,color='black',linetype='dashed',size=1)
  
  p4<- p3+
    annotate("text",x=0.75,y=0.25,
             label=paste("AUC=",round(calc_auc(p3)$AUC,2)),
             size=10,color='red')
  
  list(data=test,plot=p4)
}

SimulaROC  <-   function(){ 
  set.seed(sample(1211:176564,1))
  n <- sample(30:150,1)
  p <- runif(1,0.3,0.6)
  mD <- runif(1,50,150)
  sD <- runif(1,1,7)
  mH <- runif(1,mD+1,mD+5)
  sH <- runif(1,1,7)
  r <- Simula.Un.Biomarker.ROC(n,p,mH,sD,mD,sD)
  plot <- r$plot
  data <- r$data
  list(plot=plot,data=data)
}

 

# Define server logic required to draw a histogram
shinyServer(function(input, output) { 
  
  mostraEmpiricaD <- eventReactive(input$GoSampleEmpiric,{mostraEmpD(input$nd,input$md,input$sdd)},
                                  ignoreNULL = FALSE)
  mostraEmpiricaH <- eventReactive(input$GoSampleEmpiric,{mostraEmpH(input$nh,input$mh,input$sdh)},
                                   ignoreNULL = FALSE)
  
  newROCPlot1 <- eventReactive(input$newROC,{SimulaROC()$plot+ggtitle('Biomarker 1')+
      theme(plot.title = element_text(size = 25, face = "bold",hjust = 0.5))})
  newROCPlot2 <- eventReactive(input$newROC,{SimulaROC()$plot+ggtitle('Biomarker 2')+
      theme(plot.title = element_text(size = 25, face = "bold",hjust = 0.5))})
  
  output$plot <- renderPlot({fun(input$xc,input$mu.e,input$sd.e,input$mu.s,input$sd.s,
                                 input$xmin,input$xmax)})
  
  output$sen <- renderText({
    sen <- ifelse(input$mu.e>input$mu.s,
                  1-pnorm(input$xc,input$mu.e,input$sd.e),
                  pnorm(input$xc,input$mu.e,input$sd.e)
    )
    c(print('Sensitivity P(+/D):'),print(round(sen,3)))})
  
  output$esp <- renderText({
    esp <- ifelse(input$mu.e>input$mu.s,
                  pnorm(input$xc,input$mu.s,input$sd.s),
                  1-pnorm(input$xc,input$mu.s,input$sd.s)
    )
    c(print('Especificity P(-/H):'),print(round(esp,3)))})
  
  output$ROC <- renderPlot({
    d<-ROC(input$mu.e,input$sd.e,input$mu.s,input$sd.s,input$xmin,input$xmax)
    AUC <- Compute.AUC(input$mu.e,input$sd.e,input$mu.s,input$sd.s,input$xmin,input$xmax)
    espe <- ifelse(input$mu.e>input$mu.s,
                  pnorm(input$xc,input$mu.s,input$sd.s),
                  1-pnorm(input$xc,input$mu.s,input$sd.s))
    sens <- ifelse(input$mu.e>input$mu.s,
                 1-pnorm(input$xc,input$mu.e,input$sd.e),
                 pnorm(input$xc,input$mu.e,input$sd.e))  
   
    ggplot(d,aes(x=1-sen,y=esp))+
      geom_line(size=1.5,color='orange')+
      geom_segment(aes(x=1-sens,y=0,xend=1-sens,yend=espe),linetype='dashed')+
      geom_segment(aes(x=0,y=espe,xend= 1-sens,yend=espe),linetype='dashed')+
      geom_segment(aes(x=0,y=0,xend= 1,yend=1),linetype='dashed',color='black',size=1.5)+
      geom_point(aes(x=1-sens,y=espe),size=6,color='black',fill='white',shape=24)+
      
      scale_x_continuous(expand=c(0,0),limits=c(0,1))+
      scale_y_continuous(expand=c(0,0),limits=c(0,1))+
      ylab('Sensitivity (TPR): P(+/D)')+
      xlab('1-Especificity (FPR): P(+/H)')+
      #annotate(geom="text", x=0.75, y=0.25, label=paste("AUC:",AUC),
      #         color="red")+
     ggtitle('ROC curve')+
     theme(
        plot.title = element_text(color="#990000", size=16, face="bold"),
        panel.border = element_rect(colour = "black", fill=NA, size=2),
            axis.title.x = element_text(face="bold", colour="#990000", size=16),
            axis.text.x  = element_text(angle=0, vjust=0.5, size=12),
            axis.title.y = element_text(face="bold", colour="#990000", size=16),
            axis.text.y  = element_text(angle=0, vjust=0.5 ,size=12))+
      annotate(geom="text", x=0.65, y=0.3, label=paste('AUC:',AUC),
               color="red",size=12)+
      coord_fixed() 
  })
  
 
  
  ######################################
  
   output$Performance <- renderTable({
     sen <- input$Sensi
     esp <- input$Espe
     p <- input$Prev
     TPR <- sen
     FPR <- 1-esp
     PPV <- round(p*sen/(sen*p+(1-esp)*(1-p)),2)
     FDR <- round(1-PPV,2)
     NPV <- round(esp*(1-p)/(esp*(1-p)+(1-sen)*p),2)
     FOR <- round(1-NPV,2)
     r <-  cbind(
       c('True Positive Rate','P(+/D)',TPR,'False Positive Rate','P(+/H)',FPR),
       c('Positive Predictive Value','P(D/+)',PPV,'Negative Predictive Value','P(H/-)',NPV),
       c('False Discovery Rate','P(H/+)',FDR,'False Omision Rate','P(D/-)',FOR) 
     )
     colnames(r)<-(c('Test','Predictive','Discovery rates'))
     xtable(r)
     
   })
  
  output$PlotPV <- renderPlot({
    sen <- input$Sensi
    esp <- input$Espe
    prob <- input$Prev
    p <- ggplot(data=data.frame(x=0),aes(x=x))
     
    
    p + stat_function(fun=fPPV,args=list(sen=sen,esp=esp),xlim=c(0,1),
                      color='red',size=1)+
      stat_function(fun=fNPV,args=list(sen=sen,esp=esp),xlim=c(0,1),
                    color='blue',size=1)+
      geom_hline(yintercept = 0.5,color='black')+
      geom_segment(x=prob,y=0,xend=prob,yend=fPPV(sen,esp,prob),
                   linetype='dashed')+
      geom_segment(x=0,y=fPPV(sen,esp,prob),xend=prob,yend=fPPV(sen,esp,prob),
                   linetype='dashed')+
      geom_point(aes(x=prob,y=fPPV(sen,esp,prob)),size=4,color='red')+
      geom_segment(x=prob,y=0,xend=prob,yend=fNPV(sen,esp,prob),
                   linetype='dashed')+
      geom_segment(x=0,y=fNPV(sen,esp,prob),xend=prob,yend=fNPV(sen,esp,prob),
                   linetype='dashed')+
      geom_point(aes(x=prob,y=fNPV(sen,esp,prob)),size=4,color='blue')+
      annotate('text',x=0.75,y=0.1,label='NPV',color='blue',size=10)+
      annotate('text',x=0.25,y=0.1,label='PPV',color='red',size=10)
      })
  
   output$TableEmpirica <- renderTable({
     d <- mostraEmpiricaD()
     h <- mostraEmpiricaH()
     xc <- input$PointDiagnostic
     nnh <- input$nh
     nnd <- input$nd
     
     posH <- h %>%  filter(x>xc) %>% count() 
     posH <- round(as.numeric(posH),0)
     negH <- nnh-posH
     
     posD <- d %>%  filter(x>xc) %>% count() 
     posD <- round(as.numeric(posD),0)
     negD <- nnd-posD
     m <- matrix(c('(+)','(-)',posH,negH,posD,negD),nrow=2,byrow=F)
     colnames(m)<-c('Test','H','D')
     m
   })
   
   output$SensiEmpirica <- renderText({
     d <- mostraEmpiricaD()
     h <- mostraEmpiricaH()
     xc <- input$PointDiagnostic
     nnh <- input$nh
     nnd <- input$nd
     
     posH <- h %>%  filter(x>xc) %>% count() 
     posH <- round(as.numeric(posH),0)
     negH <- nnh-posH
     
     posD <- d %>%  filter(x>xc) %>% count() 
     posD <- round(as.numeric(posD),0)
     negD <- nnd-posD
     
     sen <- posD/nnd
     paste('Sensititiy:',round(sen,2))
   })
  
   output$EspeEmpirica <- renderText({
     d <- mostraEmpiricaD()
     h <- mostraEmpiricaH()
     xc <- input$PointDiagnostic
     nnh <- input$nh
     nnd <- input$nd
     
     posH <- h %>%  filter(x>xc) %>% count() 
     posH <- round(as.numeric(posH),0)
     negH <- nnh-posH
     
     posD <- d %>%  filter(x>xc) %>% count() 
     posD <- round(as.numeric(posD),0)
     negD <- nnd-posD
     
     esp <- negH/nnh
     paste('Especificity:',round(esp,2))
   })
   
   output$HistoEmpiric <- renderPlot({
     d <- mostraEmpiricaD()
     h <- mostraEmpiricaH()
     bw <- max(round(sd(d$x)/3),round(sd(h$x)/3))
     
     ggplot()+
       geom_histogram(data=d,aes(x=x),fill='grey',binwidth = bw,color='black')+
       geom_histogram(data=h,aes(x=x),fill='blue',binwidth = bw,color='black',alpha=0.4)+
       geom_density(data=d,aes(x=x,y=bw*..count..),size=1)+
       geom_density(data=h,aes(x=x,y=bw*..count..),size=1)+
       geom_point(aes(x=input$PointDiagnostic,y=0),size=4,shape=23,fill='white',color='black')
   })
   
   output$ROCEmpiric <- renderPlot({
     D <- mostraEmpiricaD()
     nD <- length(D$x)
      
     H <- mostraEmpiricaH()
     nH<- length(H$x)
      
     df.D <- data.frame(Status=rep(1,nD),Biomarker=D$x)
     df.H <- data.frame(Status=rep(0,nH),Biomarker=H$x)
     
     df <- rbind(df.D,df.H)
     
     p3<-ggplot(df,aes(d=Status,m=Biomarker))+
       geom_roc(color='red')+
       style_roc()+
       coord_fixed()+
       geom_segment(x=0,y=0,xend=1,yend=1,color='black',linetype='dashed',size=1)
     
     p4<- p3+
       annotate("text",x=0.75,y=0.25,
                label=paste("AUC=",round(calc_auc(p3)$AUC,2)),
                size=10,color='red')
     p4
   })
   
  output$SimulaROC <-   renderPlot({
     
    ggarrange(newROCPlot1(),  newROCPlot2(),ncol=2)
  })
   
   ###########################################################################
   ## Figures
   
   output$Concepts <- renderUI({
      
     if(input$OptConcept=='Sen')  {img(src="SensibEspecif.png",width=600,height=500)}

     else if (input$OptConcept=='Norm'){img(src="DiagnosticNormals.png",width=600,height=500)}
     
          else if (input$OptConcept=='PV'){img(src="ValorsPredictius.png",width=600,height=500)}
              else if (input$OptConcept=='ROC'){img(src="ROC.png",width='100%',height=400)}
      
     })
     
  
   
   
  
})
