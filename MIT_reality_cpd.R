#require(ecp)
#require(Rcpp)
source("depth_CPD_func.R") 
source("binary_segmentation.R") 
#source("ecp_distmat_input.R")
#source("kcp_distmat_input.R")
#sourceCpp('energyChangePoint.cpp')

MIT_data<-get(load("reality_mining_1392.RData"))

G_graph<-c()
for (i in 1:(dim(MIT_data)[3]/6)){
  m<-c(as.matrix(rowSums(MIT_data[,,(i*6-5):(i*6)],dims=2), "adjacency"))
  G_graph<-rbind(G_graph,m)
}
G_graph[G_graph >=1] <- 1
X<-G_graph
D<-as.matrix(dist(X))

minLen<-20
seed_I<-seeded.intervals(nrow(D),decay = sqrt(2))
seed_I<-seed_I[(seed_I[,2]-seed_I[,1])>minLen,]

i<-as.numeric(commandArgs(TRUE)[1])
num_permut<-1000
distmat_seed<-D[seed_I[i,1]:seed_I[i,2],seed_I[i,1]:seed_I[i,2]]
r<-depth_CPD(distmat_seed,num_permut,c=0.3)
r$loc=r$loc+seed_I[i,1]-1

path<-paste("MIT_iter_",i,'.Rdata',sep="")
save(r, file=path)
