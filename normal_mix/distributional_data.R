require("distrEx")
library("tidyr")
library(mvtnorm)
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

#setwd(dirname(rstudioapi::getSourceEditorContext()$path))
source("objTest_fctns.R")
n1=100;n2=100;n=n1+n2
num_permut<-1000
emp_power<-c()
c<-0.1
cut_off = c(0)


delta<-as.numeric(commandArgs(TRUE)[1])
monte_carlo<-as.numeric(commandArgs(TRUE)[2])

I<-diag(1,2)

# mean shift: delta in seq(0,1,0.1), 
z1<-mvrnorm(n=n1,mu=c(0,0),Sigma = 0.25*I)
z2<-mvrnorm(n=n2,mu=c(delta,0),Sigma = 0.25*I)

#scale shift: delta in seq(0,0.4,0.04), 
# z1<-mvrnorm(n=n1,mu=c(0,0),Sigma = 0.16*I)
# z2<-mvrnorm(n=n2,mu=c(0,0),Sigma = diag(c((0.4+delta)**2,0.4**2)))


df<-expand.grid(seq(-3,3,len=100),seq(-3,3,len=100))
df_matrix<-data.matrix(df)
l<-lapply(seq_len(nrow(df_matrix)), function(i) df_matrix[i,])

result_mat=list()
Data<-c()
for (i in seq_len(nrow(rbind(z1,z2)))){
  mu<-rbind(z1,z2)[i,]
  result<-sapply(l,pmvnorm,lower=-Inf,mean=mu,sigma=I)
  Data<-rbind(Data,result)
}
inc<-seq(-3,3,len=100)[2]-seq(-3,3,len=100)[1]


max_stat<-c()
for (j in 0:num_permut){
  data<-Data
  if (j!=0){
    data<-Data[sample(nrow(Data)),]
  }
  
  distmat<-as.matrix(dist(unname(data),method = "manhattan")*inc)
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
dec<-as.integer(max_stat[,1]>critical_value)
loc<-max_stat_index

#### Graph CPD
distmat<-dist( Data, method = 'manhattan' )
E1 = mstree(distmat,ngmax = 5)
n=nrow(Data)
r1 = gseg1(n,E1, statistics="all")
graph_cpd<-c()
for (k in c(1:4)){
  graph_cpd<-rbind(graph_cpd,c(r1$scanZ[[k]]$tauhat,r1$pval.appr[k]))
}


#### Energy CPD
y1 = e.divisive(X=Data,sig.lvl=0.05,R=999,k=1,min.size=30,alpha=1)
#energy_result[[i]]<-c(y1$estimates[c(-1,-length(y1$estimates))],y1$p.values[1])

result<-list()
result[[1]]<-list()
result[[2]]<-graph_cpd
result[[3]]<-c(y1$estimates[c(-1,-length(y1$estimates))],y1$p.values)
names(result)<-c("depth",'graph','energy')
result[['depth']][["1"]]=dec
result[['depth']][["2"]]=loc
result[['depth']][["3"]]=p_val
names(result[['depth']])<-c("dec",'loc','p_value')
path<-paste("delta_",delta,'_run_',monte_carlo,'.Rdata',sep="")
save(result, file=path)

