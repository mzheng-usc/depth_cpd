require(gtools)
library(pracma)
library(boot)
library(magrittr)
library(MASS)
library(energy)
library(Hotelling)
library(gTests)
library(igraph)
library(foreach)
library(doParallel)
library(matrixStats)
library(ade4)
library(mvtnorm)
require(pracma)

##### new package
require("gSeg")
library(ecp)      
library(energy)


#setwd(dirname(rstudioapi::getSourceEditorContext()$path))
source("objTest_fctns.R")
n1=100;n2=100;n=n1+n2
num_permut<-1000
emp_power<-c()
c<-0.1
cut_off = c(0)


delta<-as.numeric(commandArgs(TRUE)[1])
monte_carlo<-as.numeric(commandArgs(TRUE)[2])


# for mean_diff case
S1<-load("Sigma_30.Rdata")
S1<-get(S1)
S2<-load("Sigma_90.Rdata")
S2<-get(S2)
S3<-load("Sigma_180.Rdata")
S3<-get(S3)

#delta<-1;monte_carlo<-1
decision<-c();location<-c();p_value<-c()
graph_result<-list();energy_result<-list()

for (i in seq_along(1:3)){
  
  # There are four different cases and we have different parameters for them.
  # so we need to change the corresponding data generation method in the code and also input parameters, i.e., this .R file and parameter.sh file 
  
  # 1. mean_diff: delta in seq(0,1,0.1), 
  # 2. scale_diff: delta in seq(0,0.4,0.04), 
  # 3. mixture gaussian: delta in seq(0,1,0.1), 
  # 4. heavy tail: delta in seq(2,22,2)

  
  p=c(30,90,180)[i] # for mean_diff, scale_diff, mixture gaussian cases
  #p=c(5,15,60)[i] # for heavy tail case
  
  I<-diag(x = 1, p, p)
  
  #### 1. mean diff
  Sigma=list(S1,S2,S3)[[i]]
  Data<-rbind(mvrnorm(n1,mu=rep(0,p),Sigma=Sigma),mvrnorm(n2,mu=c(rep(delta,p)),Sigma=Sigma))
  
  
  #### 2. scale diff
  # Data<-rbind(mvrnorm(n1,mu=rep(0,p),Sigma=0.8*I),mvrnorm(n2,mu=c(rep(0,p)),Sigma=(0.8-delta)*I))
  
  
  #### 3. mixture gaussian distributions
  # A<-rbinom(n2,1,0.5)
  # mu<-c(rep(delta,0.1*p),rep(0,0.9*p))
  # Z1<-mvrnorm(n2,-mu,I);Z2<-mvrnorm(n2,mu,I)
  # Data<-rbind(mvrnorm(n1,mu=rep(0,p),Sigma=I),A*Z1+(1-A)*Z2)
  

  #### 4. heavy tailed distribution: different degrees of freedom
  # Data<-rbind(mvrnorm(n1,mu=rep(0,p),Sigma=I),matrix(rt(n2*p, df=delta),nrow=n2))
  
  max_stat<-c()
  for (j in 0:num_permut){
    data<-Data
    if (j!=0){
      data<-Data[sample(nrow(Data)),]
    }
    distmat<-as.matrix(dist( data, method = 'euclidean' ))
    test<-c()
    for (cp in seq(n*c,n*(1-c),1)){
      seg1<-data[1:cp,];seg2<-data[(cp+1):n,]
      testStat <- getT( distmat = distmat, indices = 1:n, n = nrow(seg1), m = nrow(seg2), cut_off = cut_off )
      test<-cbind(test,testStat)
    }
    max_stat<-cbind(max_stat,apply(test,1,max))
    cat(monte_carlo,'run',j,"iteration")
    
    if (j==0){
      max_stat_index<-apply(test,1,which.max)+n*c-1
    }
  }

  critical_value<-apply(max_stat[,2:ncol(max_stat)],1,quantile,probs=0.95)
  p_val<-rowSums(max_stat[,1]<max_stat[,2:ncol(max_stat)])/num_permut
  sum(max_stat[,1]>max_stat[,2:ncol(max_stat)])
  dec<-as.integer(max_stat[,1]>critical_value)
  
  decision<-cbind(decision,dec)
  location<-cbind(location,max_stat_index)
  p_value<-cbind(p_value,p_val)
  
  #### Graph CPD
  distmat<-dist( Data, method = 'euclidean' )
  E1 = mstree(distmat,ngmax = 5)
  n=nrow(Data)
  r1 = gseg1(n,E1, statistics="all")
  graph_cpd<-c()
  for (k in c(1:4)){
    graph_cpd<-rbind(graph_cpd,c(r1$scanZ[[k]]$tauhat,r1$pval.appr[k]))
  }
  graph_result[[i]]<-graph_cpd
  
  #### Energy CPD
  y1 = e.divisive(X=Data,sig.lvl=0.05,R=999,k=1,min.size=30,alpha=1)
  energy_result[[i]]<-c(y1$estimates[c(-1,-length(y1$estimates))],y1$p.values)

}
result<-list()
result[[1]]<-list()
result[[2]]<-graph_result
result[[3]]<-energy_result
names(result)<-c("depth",'graph','energy')
result[['depth']][["1"]]=decision
result[['depth']][["2"]]=location
result[['depth']][["3"]]=p_value
names(result[['depth']])<-c("dec",'loc','p_value')
#decision
#decision[i,j] # ith method, jth dimension.
path<-paste("delta_",delta,'_run_',monte_carlo,'.Rdata',sep="")
save(result, file=path)

