# function that apply KS statistics to variable splited into 2 categoris (event 1: bvar=0 and event 2: bvar=1)
# KS test
# version 1.0 created 2018.12.30
# tested variable 
# bvar - binary dependent variable 
# datascientist2020@yahoo.com

source("/Library/createdataset.R")
source("/Library/dks.R")
 
library(ROCR)
 
#parametrers of simulations
 iteracje<-40
 step1=1
 
#output 
 statystyki<-matrix(nrow=iteracje,ncol=7)
 
 #input dataset
 size<-10000
 setlevel1=0.6
 w1=1
 w2=1
 
 
 parameters_of_simulations<<-c(iteracje,step1,size,setlevel1,w1,w2)
 
 for(i in 1:iteracje) {
             i
             setlevel2<-step1*i
             createdataset(size,level1=setlevel1,level2=setlevel2,w1,w2)
           
             df1<-cbind(zz1,x1,x2)
             #model development
             model <- glm(zz1 ~ x1+x2,data=as.data.frame(df1),family=binomial(link="logit"))
             #model <- glm(zz1 ~ x1+x2,data=as.data.frame(df1),family=quasi(link="logit"))
           
             #model <- glm(zz1 ~ zmn1b+zmn2b,data=as.data.frame(df1),family=poisson(link = "log"))
             #model <- glm(zz1 ~ zmn1b+zmn2b,data=as.data.frame(df1),family=gaussian(link = "identity"))
             #summary(model)
             
             pmodel <- predict(model, newdata=as.data.frame(df1), type="response")
             
             
             #KS statistic
             #model validation
             d1<-dKS(size,x1,zz1)
             d2<-dKS(size,x2,zz1)
             d3<-dKS(size,w1*x1+w2*x2,zz1)
             d4<-dKS(size,pmodel,zz1)
             
             statystyki[i,1]<-setlevel1
             statystyki[i,2]<-setlevel2
             statystyki[i,3]<-cor(x1,x2)
             statystyki[i,4]<-d1
             statystyki[i,5]<-d2
             statystyki[i,6]<-d3
             statystyki[i,7]<-d4
         
             
             df1<-cbind(zz1,z1,x1,x2,w1*x1+w2*x2,pmodel)
             #head(df1)
           }
      
   setwd("C:/Users/Hubert/")
   write.csv(statystyki,"statystyki0107_run1.csv")
   write.csv(head(df1),"dataset0107_run1.csv")
   
   #ksidx<-statystyki[,7]/statystyki[,5]
   ksidx2<-statystyki[,7]/statystyki[,6]
   
  plot(statystyki[,2],ksidx2,col='green',xlab='Noise level',ylab='INDEX')
  lines(statystyki[,2],ksidx2,col='green')
  #points(statystyki[,2],ksidx2,col='red')
  #lines(statystyki[,2],ksidx2,col='red')
  #legend(5,1.15,c('KSindex2','KSindex'),col=c("red","green"))
