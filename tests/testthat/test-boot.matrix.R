#TESTING boot.matrix

context("boot.dispRity")

#Loading the data
load("test_data.Rda")
data<-test_data$ord_data_tips


#Testing boot.matrix with a single matrix input
bootstraps=5
rarefaction=FALSE
boot.type="full"
#Sanitizing
test_that("Sanitizing works correctly", {
    expect_error(boot.matrix(data="a", bootstraps, rarefaction))
    expect_error(boot.matrix(data, bootstraps=FALSE, rarefaction))
    expect_error(boot.matrix(data, bootstraps="a", rarefaction))
    expect_error(boot.matrix(data, bootstraps, rarefaction="a"))
})

#No bootstrap (is equal to the matrix)
test_that("No bootstraps", {
    expect_equal(sort(boot.matrix(data, bootstraps=0)[[1]][[1]][[1]][[1]]), sort(data))
})

#Rarefaction = 1, bootstraps = 5
test_that("Rarefaction is 1 + bootstraps", {
    expect_equal(length(boot.matrix(data, bootstraps)[[1]][[1]]), 1)
    expect_equal(length(boot.matrix(data, bootstraps)[[1]][[1]][[1]]), 5)
})

#Rarefaction = TRUE, bootstraps = 5
test_that("Rarefaction is TRUE + bootstraps", {
    expect_equal(length(boot.matrix(data, bootstraps, rarefaction=TRUE)[[1]][[1]]), nrow(data)-2)
    expect_equal(length(boot.matrix(data, bootstraps, rarefaction=TRUE)[[1]][[1]][[1]]), 5)
})

#Rarefaction = 5, bootstraps = 5
test_that("Rarefaction is 5 + bootstraps", {
    expect_equal(length(boot.matrix(data, bootstraps, rarefaction=5)[[1]][[1]]), 1)
    expect_equal(length(boot.matrix(data, bootstraps, rarefaction=5)[[1]][[1]][[1]]), 5)
    expect_equal(nrow(boot.matrix(data, bootstraps, rarefaction=5)[[1]][[1]][[1]][[1]]), 5)
})

#Rarefaction = 5, bootstraps = 5, boot.type
test_that("Rarefaction is 5 + bootstraps + single boot.type", {
    expect_equal(length(boot.matrix(data, bootstraps, rarefaction=5, boot.type="single")[[1]][[1]]), 1)
    expect_equal(length(boot.matrix(data, bootstraps, rarefaction=5, boot.type="single")[[1]][[1]][[1]]), 5)
    expect_equal(nrow(boot.matrix(data, bootstraps, rarefaction=5, boot.type="single")[[1]][[1]][[1]][[1]]), 5)
})

#Remove last axis
test_that("Removing the last axis works", {
    expect_equal(ncol(boot.matrix(data, bootstraps=5, rarefaction=5, rm.last.axis=TRUE)[[1]][[1]][[1]][[1]]), ncol(data)-6)
    expect_equal(ncol(boot.matrix(data, bootstraps=5, rarefaction=5, rm.last.axis=0.5)[[1]][[1]][[1]][[1]]), ncol(data)-35)
})

#Input is a time.series
source("../../R/cust.series.R")
factor<-as.data.frame(matrix(data=c(rep("series1", nrow(data)/2),rep("series2", nrow(data)/2)), nrow=nrow(data), ncol=1))
rownames(factor)<-rownames(data)
data<-cust.series(data, factor)

#Rarefaction = 1, bootstraps = 5
test_that("Rarefaction is 1 + bootstraps + series", {
    expect_equal(length(boot.matrix(data, bootstraps)[[1]][[1]]), 1)
    expect_equal(length(boot.matrix(data, bootstraps)[[1]][[1]][[1]]), 5)
})

#Rarefaction = TRUE, bootstraps = 5
test_that("Rarefaction is TRUE + bootstraps + series", {
    expect_equal(length(boot.matrix(data, bootstraps, rarefaction=TRUE)[[1]][[1]]), nrow(data[[1]])-2)
    expect_equal(length(boot.matrix(data, bootstraps, rarefaction=TRUE)[[1]][[1]][[1]]), 5)
})

#Rarefaction = 5, bootstraps = 5
test_that("Rarefaction is 5 + bootstraps + series", {
expect_equal(length(boot.matrix(data, bootstraps, rarefaction=5)[[1]][[1]]), 1)
expect_equal(length(boot.matrix(data, bootstraps, rarefaction=5)[[1]][[1]][[1]]), 5)
expect_equal(nrow(boot.matrix(data, bootstraps, rarefaction=5)[[1]][[1]][[1]][[1]]), 5)
})

#Rarefaction = 5, bootstraps = 5, boot.type
test_that("Rarefaction is 1 + bootstraps + series + single boot.type", {
expect_equal(length(boot.matrix(data, bootstraps, rarefaction=5, boot.type="single")[[1]][[1]]), 1)
expect_equal(length(boot.matrix(data, bootstraps, rarefaction=5, boot.type="single")[[1]][[1]][[1]]), 5)
expect_equal(nrow(boot.matrix(data, bootstraps, rarefaction=5, boot.type="single")[[1]][[1]][[1]][[1]]), 5)
})

#Remove last axis
test_that("Rarefaction is 1 + bootstraps + series + single boot.type + rm.last axis", {
expect_equal(ncol(boot.matrix(data, bootstraps=5, rarefaction=5, rm.last.axis=TRUE)[[1]][[1]][[1]][[1]]), ncol(data[[1]])-6)
expect_equal(ncol(boot.matrix(data, bootstraps=5, rarefaction=5, rm.last.axis=0.5)[[1]][[1]][[1]][[1]]), ncol(data[[1]])-35)
})

#Rarefaction = c(5, 8:10)
test_that("Multiple rarefaction works", {
expect_equal(length(boot.matrix(data, bootstraps, rarefaction=c(5, 8:10))), 4)
})