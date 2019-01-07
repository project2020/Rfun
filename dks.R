# function that apply KS statistics to variable splited into 2 categoris (event 1: bvar=0 and event 2: bvar=1)
# KS test
# version 1.0 created 2018.12.30
# tested variable 
# bvar - binary dependent variable 

dKS <-function(size,variable,bvar){

  
    
  i1=0
  i0=0
  
  size1=sum(ifelse((bvar==1),1,0))
  size0=sum(ifelse((bvar==0),1,0))
  
  
  ERR=0
  if ((size)<2) {ERR=1}
  if ((size)>length(variable)) {ERR=1}
  if (max(bvar)>1) {ERR=2}
  if (min(bvar)<0) {ERR=3}
  if (length(levels(as.factor(zz1)))>2) {ERR=4}
  if (size1+size0!=size) {ERR=5}
  if (ERR>0) {message("Error:", ERR, "\n")}
  
  
  if (size1!=size/2) {Warning=1}
  
  variable1<-array(size1)
  variable0<-array(size0)
  
  Fvariable1<<-matrix(2,size1)
  Fvariable0<<-matrix(2,size0)
  
  
  for(j in 1:size) 
  {
    
    if(bvar[j]==1) {
    i1=i1+1
    
    variable1[i1]<-variable[j]
    
    }
    
    else
    {
      i0=i0+1
      variable0[i0]<-variable[j]  
      
    }
  }
  
  lvar0<-length(variable0)
  lvar1<-length(variable1)
  
  variable0<<-sort(variable0)
  variable1<<-sort(variable1)
  
#cumulative %
#   for(jj in 1:lvar1) 
  # {
  #   Fvariable1[1,jj]<<-jj/lvar1
  #   Fvariable1[2,jj]<<-variable1[jj]
  #}
  #for(jj in 1:lvar0) 
  #{
  #Fvariable0[1,jj]<<-jj/lvar0
  # Fvariable0[2,jj]<<-variable0[jj]
  #}
  
  #dKS3<<-max(Fvariable1-Fvariable0)
  
  ks.test(variable0,variable1)$statistic
  
  
  
}
  
  
#example  
#dKS(10000,x1,zz1)
#dKS(50,z1,zz1)
#dKS(10000,x2,zz1)
# dKS(10000,x1+x2,zz1)

# dKS(10000,x1+2*x2,zz1)
# dKS(10000,10*x1+x2,zz1)
  
  