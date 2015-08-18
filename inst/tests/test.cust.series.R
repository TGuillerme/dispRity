#TESTING CUST.SERIES
source("../../R/sanitizing.R")
source("../../R/cust.series.R")
library(testthat)

#Change the loading data to a simplified version of Beck???
data<-matrix(data=NA, nrow=10, ncol=9)
rownames(data)<-letters[1:10]

#Sanitizing
#class
expect_error(cust.series(data, factor="A") ; message('.', appendLF=FALSE)
#same number of rows
factor<-matrix(5,5)
expect_error(cust.series(data, factor) ; message('.', appendLF=FALSE)
factor<-as.data.frame(matrix(data=c(rep(1,5), rep(2,5)), nrow=10, ncol=1))
expect_error(cust.series(data, factor) ; message('.', appendLF=FALSE)
#row names must be the same
rownames(factor)<-letters[2:11]
expect_error(cust.series(data, factor) ; message('.', appendLF=FALSE)
#One class with only 3 elements
rownames(factor)<-letters[1:10]
factor[1:2,1]<-3
expect_error(cust.series(data, factor) ; message('.', appendLF=FALSE)

#Results
factor<-as.data.frame(matrix(data=c(rep(1,5), rep(2,5)), nrow=10, ncol=1))
rownames(factor)<-letters[1:10]
test<-cust.series(data, factor)
expect_equal(length(test), 2) ; message('.', appendLF=FALSE)
expect_equal(nrow(test[[1]]), 5) ; message('.', appendLF=FALSE)