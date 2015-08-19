#TESTING TIME.SERIES
source("../../R/sanitizing.R")
source("../../R/tree.age.R")
source("../../R/tree.age_fun.R")
source("../../R/cust.series.R")
library(ape)
library(testthat)

#Loading the data
load("test_data.Rda")
tree<-test_data$tree_data
data<-test_data$ord_data_tips

#Split the data into two random custom series
factor<-as.data.frame(matrix(data=c(rep(1, nrow(data)/2),rep(2, nrow(data)/2)), nrow=nrow(data), ncol=1))
rownames(factor)<-rownames(data)
data<-cust.series(data, factor)

