#TESTING summary.dispRity

context("summary.dispRity")
# 
#Loading the data
load("test_data.Rda")
data<-test_data$ord_data_tips

#######################
#Testing
#######################

#Case 1, no bootstrap
data<-test_data$ord_data_tips
data<-dispRity(data, metric=c(sum, range))
test<-summary(data)

#Test
test_that("Works without bootstraps", {
    expect_is(test, "data.frame")
    expect_equal(nrow(test), 1)
    expect_equal(ncol(test), 3)
})

#Case 2, bootstraps
data<-test_data$ord_data_tips
data<-boot.matrix(data, bootstrap=5)
data<-dispRity(data, metric=c(sum, range))
test<-summary(data)

#Test
test_that("Works with bootstraps", {
    expect_is(test, "data.frame")
    expect_equal(nrow(test), 1)
    expect_equal(ncol(test), 7)
})

#Case 3, bootstraps + rarefaction
data<-test_data$ord_data_tips
data<-boot.matrix(data, bootstrap=5, rarefaction=c(5,6))
data<-dispRity(data, metric=c(sum, range))
test<-summary(data)

#Test
test_that("Works with bootstraps and rarefaction", {
    expect_is(test, "data.frame")
    expect_equal(nrow(test), 2)
    expect_equal(ncol(test), 7)
})

#Case 4, time series
data<-test_data$ord_data_tips
factor<-as.data.frame(matrix(data=c(rep(1, nrow(data)/2),rep(2, nrow(data)/2)), nrow=nrow(data), ncol=1))
rownames(factor)<-rownames(data)
data<-cust.series(data, factor)
data<-dispRity(data, metric=c(sum, range))
test<-summary(data)

#Test
test_that("Works with series", {
    expect_is(test, "data.frame")
    expect_equal(nrow(test), 2)
    expect_equal(ncol(test), 3)
})

#Case 5, time series + bootstraps
data<-test_data$ord_data_tips
factor<-as.data.frame(matrix(data=c(rep(1, nrow(data)/2),rep(2, nrow(data)/2)), nrow=nrow(data), ncol=1))
rownames(factor)<-rownames(data)
data<-cust.series(data, factor)
data<-boot.matrix(data, bootstrap=5)
data<-dispRity(data, metric=c(sum, range))
test<-summary(data)

#Test
test_that("Works with series and bootstraps", {
    expect_is(test, "data.frame")
    expect_equal(nrow(test), 2)
    expect_equal(ncol(test), 7)
})

#Case 5, time series + bootstraps + rarefaction
data<-test_data$ord_data_tips
factor<-as.data.frame(matrix(data=c(rep(1, nrow(data)/2),rep(2, nrow(data)/2)), nrow=nrow(data), ncol=1))
rownames(factor)<-rownames(data)
data<-cust.series(data, factor)
data<-boot.matrix(data, bootstrap=5, rarefaction=c(5,6))
data<-dispRity(data, metric=c(sum, range))
test<-summary(data)

#Test
test_that("Works with series, bootstraps and rarefaction", {
    expect_is(test, "data.frame")
    expect_equal(nrow(test), 4)
    expect_equal(ncol(test), 7)
})

