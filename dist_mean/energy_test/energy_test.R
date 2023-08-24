require("distrEx")
library("tidyr")
library(mvtnorm)
require(gtools)
library(pracma)
library(boot)
library(magrittr)
library(MASS)
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
num_iter<-50
emp_power<-c()
c<-0.1
cut_off = c(0)


delta<-as.numeric(commandArgs(TRUE)[1])
iteration<-as.numeric(commandArgs(TRUE)[2])
#rep<-as.numeric(commandArgs(TRUE)[3])
I<-diag(1,2)
energy_temp<-list()
for (iter in 1:num_iter){
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


#### Energy CPD
y1 = e.divisive(X=Data,sig.lvl=0.05,R=999,min.size=30,alpha=1)
energy_temp[[iter]]<-y1
}

path<-paste('rep_',iteration,"_delta_",delta,".Rdata",sep="")
save(energy_temp, file=path)
