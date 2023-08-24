
library(igraph)
#require("distrEx")
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
require("gSeg")
library(ecp)

#setwd(dirname(rstudioapi::getSourceEditorContext()$path))
source("objTest_fctns.R")
n1=100;n2=200;n=n1+n2
num_permut<-1000
v1<-0.0000000001
emp_power<-c()
c<-0.1
cut_off = c(0)

#delta in seq(0.0000000001,1.0000000001,0.1)
delta<-as.numeric(commandArgs(TRUE)[1])
monte_carlo<-as.numeric(commandArgs(TRUE)[2])
#delta<-1
#monte_carlo<-1


Data<-c()
for (i in 1:n1){
  #g<-sample_pa(n=200,power=v1,directed = FALSE)
  g<-as.matrix(laplacian_matrix(sample_pa(200,power=v1,directed=FALSE)))
  #m<-c(as.matrix(as.matrix(g, "adjacency")))
  Data<-rbind(Data,c(2*g[upper.tri(g)],diag(g)))
}
for (i in 1:n2){
  #g<-sample_pa(n=200,power=delta,directed = FALSE)
  #m<-c(as.matrix(as.matrix(g, "adjacency")))
  g<-as.matrix(laplacian_matrix(sample_pa(200,power=delta,directed=FALSE)))
  Data<-rbind(Data,c(2*g[upper.tri(g)],diag(g)))
}

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
y1 = e.divisive(X=Data,sig.lvl=0.05,R=999,min.size=30,alpha=1)

result<-list()
result[[1]]<-list()
result[[2]]<-r1
result[[3]]<-y1
names(result)<-c("depth",'graph','energy')
result[['depth']][["1"]]=dec
result[['depth']][["2"]]=loc
result[['depth']][["3"]]=p_val
names(result[['depth']])<-c("dec",'loc','p_value')
path<-paste("delta_",delta,'_run_',monte_carlo,'.Rdata',sep="")
save(result, file=path)






