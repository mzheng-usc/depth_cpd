source("depth_CPD_func.R")
source("binary_segmentation.R")
require(usedist)
elec<-read.csv("Net_generation_for_all_sectors.csv")
names(elec)
elec_sub<-elec[-c(8,10,13,14,16),-c(1,2,3)] 
#delete other renewables, wood and other biomass, all untility solar, pumped storage
elec_sub[elec_sub=="--"]<-0

elec_matrix<-matrix(as.numeric(unlist(elec_sub)),nrow=nrow(elec_sub))
composition_matrix<-sweep(elec_matrix,2,colSums(elec_matrix),FUN="/")
apply(composition_matrix[,c(1,2,3)],2,FUN=sum) #col sum is 1, so each col is a compositional elec_data now.
#compsition_matrix[,1]
dim(composition_matrix)
elec_data<-t(composition_matrix)
colnames(elec_data)<-c("coal","petroleum liquids","petroleum coke","natural gas","other gases","nuclear",'conventional hydro',"wind","geothermal",'biomass','other','sloar pho','scale solar')
elec_data[,2]<-elec_data[,2]+elec_data[,3]# combine petroleum
elec_data[,4]<-elec_data[,4]+elec_data[,5]# combine gases
elec_data[,8]<-elec_data[,8]+elec_data[,9]+elec_data[,10]+elec_data[,11]# renewable
elec_data[,12]<-elec_data[,12]+elec_data[,13] #solar
elec_data<-elec_data[,c(-3,-5,-9,-10,-11,-13)]

geo_dist<-function(x,y){
  acos(sum(sqrt(x)*sqrt(y)))
}

X<-elec_data
D<-as.matrix(dist_make(X,geo_dist))

minLen<-20
seed_I<-seeded.intervals(nrow(D),decay = sqrt(2))
seed_I<-seed_I[(seed_I[,2]-seed_I[,1])>minLen,]

i<-as.numeric(commandArgs(TRUE)[1])
num_permut<-1000
distmat_seed<-D[seed_I[i,1]:seed_I[i,2],seed_I[i,1]:seed_I[i,2]]
r<-depth_CPD(distmat_seed,num_permut,c=0.3)
r$loc=r$loc+seed_I[i,1]-1

path<-paste("Elec_iter_",i,'.Rdata',sep="")
save(r, file=path)

