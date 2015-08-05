#TEST PCO STD

#functions

#Testing pco.std
#example
tree<-rtree(10)
matrix<-data.frame(row.names=tree$tip.label, 'char1'=sample(c(0,1), Ntip(tree), replace=TRUE), 'char2'=sample(c(0,1), Ntip(tree), replace=TRUE))
anc.matrix<-anc.state(tree, matrix, method='ML', verbose=FALSE)

#test
#pco output

suppressWarnings(expect_is(pco.std(anc.matrix), 'pcoa')) ; message('.', appendLF=FALSE)
suppressWarnings(expect_equal(names(pco.std(anc.matrix)), c("correction","note","values","vectors","trace"))) ; message('.', appendLF=FALSE)
#errors (matrix)
suppressWarnings(expect_error(pco.std(c(1,2)))) ; message('.', appendLF=FALSE)

#errors (vegdist)
suppressWarnings(expect_error(pco.std(anc.matrix, distance="far away"))) ; message('.', appendLF=FALSE)
suppressWarnings(expect_error(pco.std(anc.matrix, na.rm="yes please"))) ; message('.', appendLF=FALSE)
#options
suppressWarnings(expect_is(pco.std(anc.matrix, diag=TRUE), 'pcoa')) ; message('.', appendLF=FALSE)

#errors (pcoa)
suppressWarnings(expect_error(pco.std(anc.matrix, correction="yes please"))) ; message('.', appendLF=FALSE)
