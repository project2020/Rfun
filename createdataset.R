# function that create random dataset consist of 4 variables
# version 1.0 created 2018.12.30
# x1 - 1'st variable
# x2 - 2'st variable vorrelated with x1 at level1
# z1 
# zz1 - binary dependent variable with occurences of event 1 at level 50%

createdataset<-function(size,level1,level2,w1,w2){

  
  # Case A corr(x_1,x_2 )=0  
  #level1<-0
  
  
  # Case B corr(x_1,x_2 )=70%  
  #level1<-0.5
  
  #level2 - noise level/unexpleind variance 
  
  # 1’st variable
  x1<<-rnorm(size)
  # 2’nd variable
  x2<<-(1-level1)*x1+level1*rnorm(size)
  
  #noise / unknown factor
  noise<-rnorm(size)
  
  w<-array(3)
  w[1]<-w1
  w[2]<-w2
  
  #dependent variable (Probability)
  z1<<-1/(1+exp((w[1]*x1+w[2]*x2+level2*noise)))
  
  #binary dependent variable with 50% occurences of 1
  zz1<<-ifelse(z1 > median(z1),1,0)
  
}

#createdataset(size=10000,level1=0,level2=1,w1=1,w2=1)
#example 
#createdataset(size=50,level1=0.5,level2=1,w1=1,w2=1)
