#TESTING dispRity

context("dispRity")

#Loading the data
load("test_data.Rda")
data<-test_data$ord_data_tips

#Sanitizing
test_that("Sanitizing works", {
    expect_error(dispRity(data="a", c(sum, ranges), FALSE))
    expect_error(dispRity(data=1, c(sum, ranges), FALSE))
    expect_error(dispRity(data, metric="a", FALSE))
    expect_error(dispRity(data, metric=1, FALSE))
    expect_error(dispRity(data, metric=sum, FALSE))
    expect_error(dispRity(data, metric=c(1,2), FALSE))
    expect_error(dispRity(data, metric=c(sum, 1), FALSE))
    expect_error(dispRity(data, c(sum, ranges), verbose="yes"))
})
#Reset
test <- NULL ; data<-test_data$ord_data_tips

#one matrix
test<-dispRity(data, metric=c(sum, ranges))
test_that("dispRity works with a single matrix", {
    expect_is(test, "dispRity")
    expect_equal(names(test), c("data","disparity","elements","series","call"))
    expect_is(test$data$observed, "list")
    expect_equal(length(test$series), 1)
    expect_equal(length(test$elements), 50)
})
#Reset
test <- NULL ; data<-test_data$ord_data_tips

#bootstrapped
data<-boot.matrix(data, bootstrap=5, rarefaction=FALSE, boot.type="full")
test<-dispRity(data, metric=c(sum, ranges))
test_that("dispRity works with a bootstrapped matrix", {
    expect_is(test, "dispRity")
    expect_equal(names(test), c("data","disparity","elements","series","call"))
    expect_is(test$data$bootstraps, "list")
    expect_equal(length(test$series), 1)
    expect_equal(length(test$elements), 50)
})
#Reset
test <- NULL ; data<-test_data$ord_data_tips

#bootstrapped + rarefied
data<-test_data$ord_data_tips
data<-boot.matrix(data, bootstrap=5, rarefaction=TRUE, boot.type="full")
test<-dispRity(data, metric=c(sum, ranges))
test_that("dispRity works with a bootstrapped and rarefied matrix", {
    expect_is(test, "dispRity")
    expect_equal(names(test), c("data","disparity","elements","series","call"))
    expect_is(test$data$bootstraps, "list")
    expect_equal(length(test$series), 1)
    expect_equal(length(test$elements), 50)
})
#Reset
test <- NULL ; data<-test_data$ord_data_tips

#one matrix with series
data<-test_data$ord_data_tips
factor<-as.data.frame(matrix(data=c(rep(1, nrow(data)/2),rep(2, nrow(data)/2)), nrow=nrow(data), ncol=1))
rownames(factor)<-rownames(data)
data<-cust.series(data, factor)
test<-dispRity(data, metric=c(sum, ranges))
test_that("dispRity works with custom series", {
    expect_is(test, "dispRity")
    expect_equal(names(test), c("data","disparity","elements","series","call"))
    expect_is(test$data$observed, "list")
    expect_equal(length(test$series), 2)
    expect_equal(length(test$elements), 50)
})
#Reset
test <- NULL ; data<-test_data$ord_data_tips

#bootstrapped + rarefied + series
factor<-as.data.frame(matrix(data=c(rep(1, nrow(data)/2),rep(2, nrow(data)/2)), nrow=nrow(data), ncol=1))
rownames(factor)<-rownames(data)
data<-cust.series(data, factor)
data<-boot.matrix(data, bootstrap=5, rarefaction=FALSE, boot.type="full")
test<-dispRity(data, metric=c(sum, ranges))
test_that("dispRity works with a bootstrapped, rarefied, custom series", {
    expect_is(test, "dispRity")
    expect_equal(names(test), c("data","disparity","elements","series","call"))
    expect_is(test$data$bootstraps, "list")
    expect_equal(length(test$series), 2)
    expect_equal(length(test$elements), 50)
})
#Reset
test <- NULL ; data<-test_data$ord_data_tips

#testing example
test_that("Example works", {
    data(BeckLee_mat50)

    sum_of_ranges <- dispRity(BeckLee_mat50, metric = c(sum, ranges))
    ex1<-summary(sum_of_ranges)
    expect_is(ex1, "data.frame")
    expect_equal(dim(ex1), c(1,3))

    bootstrapped_data <- boot.matrix(BeckLee_mat50, bootstraps=100)
    ex2<-dispRity(bootstrapped_data, metric=c(sum, ranges))
    expect_is(ex2, "dispRity")
    expect_equal(dim(ex2[[1]][[1]][[1]][[1]][[1]]), c(50,48))

    factors <- as.data.frame(matrix(data = c(rep(1, nrow(BeckLee_mat50)/2), rep(2, nrow(BeckLee_mat50)/2)), nrow = nrow(BeckLee_mat50), ncol = 1, dimnames = list(rownames(BeckLee_mat50))))
    customised_series <- cust.series(BeckLee_mat50, factors)
    set.seed(1)
    bootstrapped_data <- boot.matrix(customised_series, bootstraps=100)
    sum_of_ranges <- dispRity(bootstrapped_data, metric=c(sum, ranges))
    ex3<-summary(sum_of_ranges)
    expect_is(ex3, "data.frame")
    expect_equal(ex3[,4], c(32.67,33.85))
})
#Reset
test <- NULL ; data<-test_data$ord_data_tips