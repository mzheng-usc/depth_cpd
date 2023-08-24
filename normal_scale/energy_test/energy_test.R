library(pracma)
library(boot)
library(magrittr)
library(MASS)
#library(energy)
library(Hotelling)
library(gTests)
library(igraph)
library(foreach)
library(doParallel)
library(matrixStats)
library(ade4)
library(mvtnorm)
#Rscript -e 'install.packages(c("gSeg","ecp"), repos="https://cloud.r-project.org")'
##### new package
require("gSeg")
library(ecp)
#install.packages(c("foreach","doParallel","matrixStats","ade4","mvtnorm","gSeg","ecp"))

#setwd(dirname(rstudioapi::getSourceEditorContext()$path))
source("objTest_fctns.R")
n1=100;n2=200;n=n1+n2
num_iter<-500
emp_power<-c()
c<-0.1
cut_off = c(0)


delta<-as.numeric(commandArgs(TRUE)[1])
i<-as.numeric(commandArgs(TRUE)[2])

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


  
  # There are four different cases and we have different parameters for them.
  # so we need to change the corresponding data generation method in the code and also input parameters, i.e., this .R file and parameter.sh file 
  
  # 1. mean_diff: delta in seq(0,1,0.1), 
  # 2. scale_diff: delta in seq(0,0.4,0.04), 
  # 3. mixture gaussian: delta in seq(0,1,0.1), 
  # 4. heavy tail: delta in seq(2,22,2)
  
  
p=c(30,90,180)[i] # for mean_diff, scale_diff, mixture gaussian cases
#p=c(5,15,60)[i] # for heavy tail case

I<-diag(x = 1, p, p)

energy_temp<-list()
for (iter in 1:num_iter){
#### 1. mean diff
#Sigma=list(S1,S2,S3)[[i]]
#Data<-rbind(mvrnorm(n1,mu=rep(0,p),Sigma=Sigma),mvrnorm(n2,mu=c(rep(delta,p)),Sigma=Sigma))


#### 2. scale diff
Data<-rbind(mvrnorm(n1,mu=rep(0,p),Sigma=0.8*I),mvrnorm(n2,mu=c(rep(0,p)),Sigma=(0.8-delta)*I))


#### 3. mixture gaussian distributions
# A<-rbinom(n2,1,0.5)
# mu<-c(rep(delta,0.1*p),rep(0,0.9*p))
# Z1<-mvrnorm(n2,-mu,I);Z2<-mvrnorm(n2,mu,I)
# Data<-rbind(mvrnorm(n1,mu=rep(0,p),Sigma=I),A*Z1+(1-A)*Z2)


#### 4. heavy tailed distribution: different degrees of freedom
# Data<-rbind(mvrnorm(n1,mu=rep(0,p),Sigma=I),matrix(rt(n2*p, df=delta),nrow=n2))
#### Energy CPD
y1 = e.divisive(X=Data,sig.lvl=0.05,R=999,min.size=30,alpha=1)
energy_temp[[iter]]<-y1
}

path<-paste('dim_',i,"_delta_",delta,'.Rdata',sep="")
save(energy_temp, file=path)


