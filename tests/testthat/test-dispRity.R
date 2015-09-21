#TESTING dispRity

context("dispRity")

#Loading the data
load("test_data.Rda")
data<-test_data$ord_data_tips

#Sanitizing
test_that("Sanitizing works", {
    expect_error(dispRity(data="a", c(sum, range), FALSE))
    expect_error(dispRity(data=1, c(sum, range), FALSE))
    expect_error(dispRity(data, metric="a", FALSE))
    expect_error(dispRity(data, metric=1, FALSE))
    expect_error(dispRity(data, metric=sum, FALSE))
    expect_error(dispRity(data, metric=c(1,2), FALSE))
    expect_error(dispRity(data, metric=c(sum, 1), FALSE))
    expect_error(dispRity(data, c(sum, range), verbose="yes"))
})

#one matrix
test<-dispRity(data, metric=c(sum, range))
test_that("dispRity works with a single matrix", {
    expect_is(test, "dispRity")
    expect_equal(names(test), c("matrix","disparity","taxa","series","call"))
    expect_is(test$matrix, "list")
    expect_equal(length(test$series), 1)
    expect_equal(length(test$taxa), 50)
})

#bootstrapped
data<-boot.matrix(data, bootstrap=5, rarefaction=FALSE, boot.type="full")
test<-dispRity(data, metric=c(sum, range))
test_that("dispRity works with a bootstrapped matrix", {
    expect_is(test, "dispRity")
    expect_equal(names(test), c("bootstraps","disparity","taxa","series","call"))
    expect_is(test$bootstraps, "list")
    expect_equal(length(test$series), 1)
    expect_equal(length(test$taxa), 50)
})

#bootstrapped + rarefied
data<-test_data$ord_data_tips
data<-boot.matrix(data, bootstrap=5, rarefaction=TRUE, boot.type="full")
test<-dispRity(data, metric=c(sum, range))
test_that("dispRity works with a bootstrapped and rarefied matrix", {
    expect_is(test, "dispRity")
    expect_equal(names(test), c("bootstraps","disparity","taxa","series","call"))
    expect_is(test$bootstraps, "list")
    expect_equal(length(test$series), 1)
    expect_equal(length(test$taxa), 50)
})

#one matrix with series
data<-test_data$ord_data_tips
factor<-as.data.frame(matrix(data=c(rep(1, nrow(data)/2),rep(2, nrow(data)/2)), nrow=nrow(data), ncol=1))
rownames(factor)<-rownames(data)
data<-cust.series(data, factor)
test<-dispRity(data, metric=c(sum, range))
test_that("dispRity works with custom series", {
    expect_is(test, "dispRity")
    expect_equal(names(test), c("matrix","disparity","taxa","series","call"))
    expect_is(test$matrix, "list")
    expect_equal(length(test$series), 2)
    expect_equal(length(test$taxa), 50)
})

#bootstrapped + rarefied + series
data<-boot.matrix(data, bootstrap=5, rarefaction=FALSE, boot.type="full")
test<-dispRity(data, metric=c(sum, range))
test_that("dispRity works with a bootstrapped, rarefied, custom series", {
    expect_is(test, "dispRity")
    expect_equal(names(test), c("bootstraps","disparity","taxa","series","call"))
    expect_is(test$bootstraps, "list")
    expect_equal(length(test$series), 2)
    expect_equal(length(test$taxa), 50)
})