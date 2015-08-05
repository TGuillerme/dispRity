#TEST ANC.UNC

#functions

#Testing anc.unc
#example
tree<-rtree(10)
matrix.state<-data.frame(row.names=tree$tip.label, 'char1'=sample(c(0,1), Ntip(tree), replace=TRUE), 'char2'=sample(c(0,1), Ntip(tree), replace=TRUE))
set.seed(10)
#prob matrix with only two cells == 1
matrix.prob<-data.frame(row.names=tree$tip.label, "char1"=rnorm(10), "char2"=rnorm(10))
matrix.prob=matrix.prob^2 ; matrix.prob[,1]<-matrix.prob[,1]/max(matrix.prob[,1]) ; matrix.prob[,2]<-matrix.prob[,2]/max(matrix.prob[,2])
anc.mat<-list("state"=matrix.state, "prob"=matrix.prob, "rate"=NA)
test<-anc.unc(anc.mat, threshold=1)

#test
#anc.mat
expect_is(anc.mat, 'list') ; message('.', appendLF=FALSE)
expect_that(length(anc.mat), equals(3)) ; message('.', appendLF=FALSE)
expect_that(names(anc.mat), equals(c("state", "prob", "rate"))) ; message('.', appendLF=FALSE)

#object
expect_is(test, 'list') ; message('.', appendLF=FALSE)
expect_that(length(test), equals(3)) ; message('.', appendLF=FALSE)
expect_that(names(test), equals(c("state", "prob", "rate"))) ; message('.', appendLF=FALSE)

#only two cells in the right threshold
expect_equal(which(is.numeric(suppressWarnings(as.numeric(test$state[,1])))), 1) ; message('.', appendLF=FALSE)
expect_true(any(match("?", test$state[,1]))) ; message('.', appendLF=FALSE)
expect_equal(which(is.numeric(suppressWarnings(as.numeric(test$state[,2])))), 1) ; message('.', appendLF=FALSE)
expect_true(any(match("?", test$state[,2]))) ; message('.', appendLF=FALSE)

#errors
expect_error(anc.unc(anc.mat, threshold='YES')) ; message('.', appendLF=FALSE)
expect_error(anc.unc(anc.mat, threshold=1.8)) ; message('.', appendLF=FALSE)
expect_error(anc.unc("hahaha")) ; message('.', appendLF=FALSE)

