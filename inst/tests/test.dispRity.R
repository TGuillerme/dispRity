#TESTING DISPRITY
source("../../R/sanitizing.R")
source("../../R/boot.matrix.R")
source("../../R/boot.matrix_fun.R")
source("../../R/print.dispRity.R")

library(ape)
library(testthat)

#Loading the data
load("test_data.Rda")
data<-test_data$ord_data_tips

#load the functions
source("../../R/dispRity.metric.R")
source("../../R/dispRity.R")
source("../../R/dispRity_fun.R")

#Sanitizing
expect_error(dispRity(data="a", c(sum, range), FALSE)) ; message('.', appendLF=FALSE)
expect_error(dispRity(data=1, c(sum, range), FALSE)) ; message('.', appendLF=FALSE)
expect_error(dispRity(data, metric="a", FALSE)) ; message('.', appendLF=FALSE)
expect_error(dispRity(data, metric=1, FALSE)) ; message('.', appendLF=FALSE)
expect_error(dispRity(data, metric=sum, FALSE)) ; message('.', appendLF=FALSE)
expect_error(dispRity(data, metric=c(1,2), FALSE)) ; message('.', appendLF=FALSE)
expect_error(dispRity(data, metric=c(sum, 1), FALSE)) ; message('.', appendLF=FALSE)
expect_error(dispRity(data, c(sum, range), verbose="yes")) ; message('.', appendLF=FALSE)

#one matrix
test<-dispRity(data, metric=c(sum, range))
expect_is(test, "dispRity") ; message('.', appendLF=FALSE)
expect_equal(names(test), c("matrix","disparity","taxa","series","call")) ; message('.', appendLF=FALSE)
expect_is(test$matrix, "list") ; message('.', appendLF=FALSE)
expect_equal(length(test$series), 1) ; message('.', appendLF=FALSE)
expect_equal(length(test$taxa), 50) ; message('.', appendLF=FALSE)

#bootstrapped
data<-boot.matrix(data, bootstrap=5, rarefaction=FALSE, boot.type="full")
test<-dispRity(data, metric=c(sum, range))
expect_is(test, "dispRity") ; message('.', appendLF=FALSE)
expect_equal(names(test), c("bootstraps","disparity","taxa","series","call")) ; message('.', appendLF=FALSE)
expect_is(test$bootstraps, "list") ; message('.', appendLF=FALSE)
expect_equal(length(test$series), 1) ; message('.', appendLF=FALSE)
expect_equal(length(test$taxa), 50) ; message('.', appendLF=FALSE)

#bootstrapped + rarefied
data<-test_data$ord_data_tips
data<-boot.matrix(data, bootstrap=5, rarefaction=TRUE, boot.type="full")
test<-dispRity(data, metric=c(sum, range))
expect_is(test, "dispRity") ; message('.', appendLF=FALSE)
expect_equal(names(test), c("bootstraps","disparity","taxa","series","call")) ; message('.', appendLF=FALSE)
expect_is(test$bootstraps, "list") ; message('.', appendLF=FALSE)
expect_equal(length(test$series), 1) ; message('.', appendLF=FALSE)
expect_equal(length(test$taxa), 50) ; message('.', appendLF=FALSE)

#one matrix with series
source("../../R/cust.series.R")
data<-test_data$ord_data_tips
factor<-as.data.frame(matrix(data=c(rep(1, nrow(data)/2),rep(2, nrow(data)/2)), nrow=nrow(data), ncol=1))
rownames(factor)<-rownames(data)
data<-cust.series(data, factor)
test<-dispRity(data, metric=c(sum, range))
expect_is(test, "dispRity") ; message('.', appendLF=FALSE)
expect_equal(names(test), c("matrix","disparity","taxa","series","call")) ; message('.', appendLF=FALSE)
expect_is(test$matrix, "list") ; message('.', appendLF=FALSE)
expect_equal(length(test$series), 2) ; message('.', appendLF=FALSE)
expect_equal(length(test$taxa), 50) ; message('.', appendLF=FALSE)

#bootstrapped + rarefied + series
data<-boot.matrix(data, bootstrap=5, rarefaction=FALSE, boot.type="full")
test<-dispRity(data, metric=c(sum, range))
expect_is(test, "dispRity") ; message('.', appendLF=FALSE)
expect_equal(names(test), c("bootstraps","disparity","taxa","series","call")) ; message('.', appendLF=FALSE)
expect_is(test$bootstraps, "list") ; message('.', appendLF=FALSE)
expect_equal(length(test$series), 2) ; message('.', appendLF=FALSE)
expect_equal(length(test$taxa), 50) ; message('.', appendLF=FALSE)
