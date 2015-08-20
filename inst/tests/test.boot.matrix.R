#TESTING TIME.SERIES
source("../../R/sanitizing.R")
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
