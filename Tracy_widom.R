#9-5-12
#Script for running tracy widom



TWcalc<-function(dat,k){
  x<-dat
  y<-x%*%t(x)
  eig<-eigen(y)
  eig<-eig$values
  eig<-eig[1:(length(eig))]
  n<-ncol(x)
  m0=nrow(x)
  k=k
  TWres_<-c()
  for (i in 1:k){
    m=(m0)-i+1
    m=m
    eiv<-eig[i:(m-1)]
    #n_=n
    n_=((m+1)*sum(eiv)^2)/((m-1)*sum(eiv^2)-sum(eiv)^2)
    S=((sqrt(n_-1)+sqrt(m)))/n_*(1/sqrt(n_-1)+1/sqrt(m))^(1/3)
    u_=(sqrt(n_-1)+sqrt(m))^2/n_
    L1<-(m-1)*eiv[1]/sum(eiv)
    TW_<-(L1-u_)/S
    TWres_<-c(TWres_,TW_)
  }