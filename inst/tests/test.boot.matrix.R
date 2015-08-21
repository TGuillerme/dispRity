#TESTING BOOT.MATRIX
source("../../R/sanitizing.R")
source("../../R/print.dispRity.R")
library(ape)
library(testthat)

#Loading the data
load("test_data.Rda")
tree<-test_data$tree_data
data<-test_data$ord_data_tips

#Testing boot.matrix with a single matrix input
source("../../R/boot.matrix.R")
source("../../R/boot.matrix_fun.R")
bootstraps=5
rarefaction=FALSE
boot.type="full"
#Sanitizing
expect_error(boot.matrix(data="a", bootstraps, rarefaction)); message('.', appendLF=FALSE)
expect_error(boot.matrix(data, bootstraps=FALSE, rarefaction)); message('.', appendLF=FALSE)
expect_error(boot.matrix(data, bootstraps="a", rarefaction)); message('.', appendLF=FALSE)
expect_error(boot.matrix(data, bootstraps, rarefaction="a")); message('.', appendLF=FALSE)

#No bootstrap (is equal to the matrix)
expect_equal(sort(boot.matrix(data, bootstraps=0)[[1]][[1]][[1]][[1]]), sort(data)); message('.', appendLF=FALSE)

#Rarefaction = 1, bootstraps = 5
expect_equal(length(boot.matrix(data, bootstraps)[[1]][[1]]), 1); message('.', appendLF=FALSE)
expect_equal(length(boot.matrix(data, bootstraps)[[1]][[1]][[1]]), 5); message('.', appendLF=FALSE)

#Rarefaction = TRUE, bootstraps = 5
expect_equal(length(boot.matrix(data, bootstraps, rarefaction=TRUE)[[1]][[1]]), nrow(data)-2); message('.', appendLF=FALSE)
expect_equal(length(boot.matrix(data, bootstraps, rarefaction=TRUE)[[1]][[1]][[1]]), 5); message('.', appendLF=FALSE)

#Rarefaction = 5, bootstraps = 5
expect_equal(length(boot.matrix(data, bootstraps, rarefaction=5)[[1]][[1]]), 1); message('.', appendLF=FALSE)
expect_equal(length(boot.matrix(data, bootstraps, rarefaction=5)[[1]][[1]][[1]]), 5); message('.', appendLF=FALSE)
expect_equal(nrow(boot.matrix(data, bootstraps, rarefaction=5)[[1]][[1]][[1]][[1]]), 5); message('.', appendLF=FALSE)

#Rarefaction = 5, bootstraps = 5, boot.type
expect_equal(length(boot.matrix(data, bootstraps, rarefaction=5, boot.type="single")[[1]][[1]]), 1); message('.', appendLF=FALSE)
expect_equal(length(boot.matrix(data, bootstraps, rarefaction=5, boot.type="single")[[1]][[1]][[1]]), 5); message('.', appendLF=FALSE)
expect_equal(nrow(boot.matrix(data, bootstraps, rarefaction=5, boot.type="single")[[1]][[1]][[1]][[1]]), 5); message('.', appendLF=FALSE)

#Remove last axis
expect_equal(ncol(boot.matrix(data, bootstraps=5, rarefaction=5, rm.last.axis=TRUE)[[1]][[1]][[1]][[1]]), ncol(data)-6); message('.', appendLF=FALSE)
expect_equal(ncol(boot.matrix(data, bootstraps=5, rarefaction=5, rm.last.axis=0.5)[[1]][[1]][[1]][[1]]), ncol(data)-35); message('.', appendLF=FALSE)


#Input is a time.series
source("../../R/cust.series.R")
factor<-as.data.frame(matrix(data=c(rep("series1", nrow(data)/2),rep("series2", nrow(data)/2)), nrow=nrow(data), ncol=1))
rownames(factor)<-rownames(data)
data<-cust.series(data, factor)

#Rarefaction = 1, bootstraps = 5
expect_equal(length(boot.matrix(data, bootstraps)[[1]][[1]]), 1); message('.', appendLF=FALSE)
expect_equal(length(boot.matrix(data, bootstraps)[[1]][[1]][[1]]), 5); message('.', appendLF=FALSE)

#Rarefaction = TRUE, bootstraps = 5
expect_equal(length(boot.matrix(data, bootstraps, rarefaction=TRUE)[[1]][[1]]), nrow(data[[1]])-2); message('.', appendLF=FALSE)
expect_equal(length(boot.matrix(data, bootstraps, rarefaction=TRUE)[[1]][[1]][[1]]), 5); message('.', appendLF=FALSE)

#Rarefaction = 5, bootstraps = 5
expect_equal(length(boot.matrix(data, bootstraps, rarefaction=5)[[1]][[1]]), 1); message('.', appendLF=FALSE)
expect_equal(length(boot.matrix(data, bootstraps, rarefaction=5)[[1]][[1]][[1]]), 5); message('.', appendLF=FALSE)
expect_equal(nrow(boot.matrix(data, bootstraps, rarefaction=5)[[1]][[1]][[1]][[1]]), 5); message('.', appendLF=FALSE)

#Rarefaction = 5, bootstraps = 5, boot.type
expect_equal(length(boot.matrix(data, bootstraps, rarefaction=5, boot.type="single")[[1]][[1]]), 1); message('.', appendLF=FALSE)
expect_equal(length(boot.matrix(data, bootstraps, rarefaction=5, boot.type="single")[[1]][[1]][[1]]), 5); message('.', appendLF=FALSE)
expect_equal(nrow(boot.matrix(data, bootstraps, rarefaction=5, boot.type="single")[[1]][[1]][[1]][[1]]), 5); message('.', appendLF=FALSE)

#Remove last axis
expect_equal(ncol(boot.matrix(data, bootstraps=5, rarefaction=5, rm.last.axis=TRUE)[[1]][[1]][[1]][[1]]), ncol(data[[1]])-6); message('.', appendLF=FALSE)
expect_equal(ncol(boot.matrix(data, bootstraps=5, rarefaction=5, rm.last.axis=0.5)[[1]][[1]][[1]][[1]]), ncol(data[[1]])-35); message('.', appendLF=FALSE)