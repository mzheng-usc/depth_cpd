#!/bin/bash
tar -xzf R413.tar.gz
# (optional) if you have a set of packages (created in Part 1), untar them also
tar -xzf R_packages.tar.gz
export PATH=$PWD/R/bin:$PATH
export RHOME=$PWD/R
export R_LIBS=$PWD/packages
# run your script
Rscript -e 'install.packages(c("gSeg","ecp"), repos="https://cloud.r-project.org")'
ls
cat network_sim.R 
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
num_permut<-10
v1<-0.0000000001
emp_power<-c()
c<-0.1
cut_off = c(0)
R
ls
exit
