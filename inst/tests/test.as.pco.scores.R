#TEST AS PCO SCORES

#functions

#Testing pco.std
#example
set.seed(1)
tr<-rtree(10)
set.seed(1)
tree<-rtree(10)
tree$node.label<-paste("n", seq(1:9), sep="")
matrix<-data.frame(row.names=tree$tip.label, 'char1'=sample(c(0,1), Ntip(tree), replace=TRUE), 'char2'=sample(c(0,1), Ntip(tree), replace=TRUE))
anc.matrix<-anc.state(tree, matrix, method='ML', verbose=FALSE)
tax0<-"whatever"
tax1<-list("t9610"=12, "t1273845"=14)
tax2<-list("t127"=15, "t34568910"=c(11, 15))
pco<-pco.std(anc.matrix)

#test
#pco output
expect_is(as.pco.scores(tree, pco), 'pco.scores') ; message('.', appendLF=FALSE)
expect_is(as.pco.scores(tree, pco, taxonomy.list=tax1), 'pco.scores') ; message('.', appendLF=FALSE)
expect_is(as.pco.scores(tree, pco, taxonomy.list=tax2), 'pco.scores') ; message('.', appendLF=FALSE)
expect_is(as.pco.scores(tree, pco, n.axis=2), 'pco.scores') ; message('.', appendLF=FALSE)
expect_equal(names(as.pco.scores(tree, pco)), c("scores")) ; message('.', appendLF=FALSE)
expect_equal(names(as.pco.scores(tree, pco, taxonomy.list=tax1)), c("scores","taxonomy")) ; message('.', appendLF=FALSE)
expect_equal(names(as.pco.scores(tree, pco, taxonomy.list=tax2)), c("scores","taxonomy")) ; message('.', appendLF=FALSE)

#error (no node labels)
expect_error(as.pco.scores(tr, prco)) ; message('.', appendLF=FALSE)

#error (axis)
expect_error(as.pco.scores(tree, pco, n.axis=1)) ; message('.', appendLF=FALSE)
expect_error(as.pco.scores(tree, pco, n.axis=3)) ; message('.', appendLF=FALSE)

#error (taxonomy)
expect_error(as.pco.scores(tree, pco, taxonomy.list=tax0)) ; message('.', appendLF=FALSE)
expect_error(as.pco.scores(tree, pco, taxonomy.list=list(NA))) ; message('.', appendLF=FALSE)
