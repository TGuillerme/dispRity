#TESTING DISPRITY
source("../../R/sanitizing.R")
source("../../R/boot.matrix.R")
source("../../R/boot.matrix_fun.R")
source("../../R/print.dispRity.R")
source("../../R/dispRity.metric.R")
source("../../R/dispRity.R")
source("../../R/dispRity_fun.R")
source("../../R/cust.series.R")
source("../../R/summary.dispRity.R")
source("../../R/summary.dispRity_fun.R")

#source("../../R/plot.dispRity.R")
#source("../../R/plot.dispRity_fun.R")

library(ape)
library(testthat)

#Loading the data
load("test_data.Rda")
data<-test_data$ord_data_tips

#Testing

#Case 1, no bootstrap
data<-test_data$ord_data_tips
data<-dispRity(data, metric=c(sum, range))
test<-summary(data)
#test
expect_is(test, "data.frame") ; message('.', appendLF=FALSE)
expect_equal(nrow(test), 1) ; message('.', appendLF=FALSE)
expect_equal(ncol(test), 3) ; message('.', appendLF=FALSE)

#Case 2, bootstraps
data<-test_data$ord_data_tips
data<-boot.matrix(data, bootstrap=5)
data<-dispRity(data, metric=c(sum, range))
test<-summary(data)
#test
expect_is(test, "data.frame") ; message('.', appendLF=FALSE)
expect_equal(nrow(test), 1) ; message('.', appendLF=FALSE)
expect_equal(ncol(test), 7) ; message('.', appendLF=FALSE)

#Case 3, bootstraps + rarefaction
data<-test_data$ord_data_tips
data<-boot.matrix(data, bootstrap=5, rarefaction=c(5,6))
data<-dispRity(data, metric=c(sum, range))
test<-summary(data)
#test
expect_is(test, "data.frame") ; message('.', appendLF=FALSE)
expect_equal(nrow(test), 2) ; message('.', appendLF=FALSE)
expect_equal(ncol(test), 7) ; message('.', appendLF=FALSE)

#Case 4, time series
data<-test_data$ord_data_tips
factor<-as.data.frame(matrix(data=c(rep(1, nrow(data)/2),rep(2, nrow(data)/2)), nrow=nrow(data), ncol=1))
rownames(factor)<-rownames(data)
data<-cust.series(data, factor)
data<-dispRity(data, metric=c(sum, range))
test<-summary(data)
#test
expect_is(test, "data.frame") ; message('.', appendLF=FALSE)
expect_equal(nrow(test), 2) ; message('.', appendLF=FALSE)
expect_equal(ncol(test), 3) ; message('.', appendLF=FALSE)

#Case 5, time series + bootstraps
data<-test_data$ord_data_tips
factor<-as.data.frame(matrix(data=c(rep(1, nrow(data)/2),rep(2, nrow(data)/2)), nrow=nrow(data), ncol=1))
rownames(factor)<-rownames(data)
data<-cust.series(data, factor)
data<-boot.matrix(data, bootstrap=5)
data<-dispRity(data, metric=c(sum, range))
test<-summary(data)
#test
expect_is(test, "data.frame") ; message('.', appendLF=FALSE)
expect_equal(nrow(test), 2) ; message('.', appendLF=FALSE)
expect_equal(ncol(test), 7) ; message('.', appendLF=FALSE)


#Case 5, time series + bootstraps + rarefaction
data<-test_data$ord_data_tips
factor<-as.data.frame(matrix(data=c(rep(1, nrow(data)/2),rep(2, nrow(data)/2)), nrow=nrow(data), ncol=1))
rownames(factor)<-rownames(data)
data<-cust.series(data, factor)
data<-boot.matrix(data, bootstrap=5, rarefaction=c(5,6))
data<-dispRity(data, metric=c(sum, range))
test<-summary(data)
#test
expect_is(test, "data.frame") ; message('.', appendLF=FALSE)
expect_equal(nrow(test), 4) ; message('.', appendLF=FALSE)
expect_equal(ncol(test), 7) ; message('.', appendLF=FALSE)
