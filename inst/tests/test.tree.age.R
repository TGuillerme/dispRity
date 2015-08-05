#TEST TREE AGE

#functions

#Testing tree.age_table
#example
tree<-rtree(10)
table_1<-tree.age_table(tree)

#test
#class
expect_is(table_1, 'data.frame') ; message('.', appendLF=FALSE)
#col.names
expect_that(colnames(table_1), equals(c('ages', 'edges'))) ; message('.', appendLF=FALSE)
#row lengths
expect_that(nrow(table_1), equals(Ntip(tree)+Nnode(tree))) ; message('.', appendLF=FALSE)
#ages are numeric
expect_is(table_1[,1], 'numeric') ; message('.', appendLF=FALSE)
#edges are factors
suppressWarnings({
same_names<-length(which(as.character(table_1[,2]) == as.character(tree$tip.label)))})
expect_equal(same_names, Ntip(tree)) ; message('.', appendLF=FALSE)


#Testing tree.age_table
#example
tree<-rtree(10)
table_1<-tree.age_table(tree)

#test
#class
expect_is(table_1, 'data.frame') ; message('.', appendLF=FALSE)
#col.names
expect_that(colnames(table_1), equals(c('ages', 'edges'))) ; message('.', appendLF=FALSE)
#row lengths
expect_that(nrow(table_1), equals(Ntip(tree)+Nnode(tree))) ; message('.', appendLF=FALSE)
#ages are numeric
expect_is(table_1[,1], 'numeric') ; message('.', appendLF=FALSE)
#edges are factors
suppressWarnings({
same_names<-length(which(as.character(table_1[,2]) == as.character(tree$tip.label)))})
expect_equal(same_names, Ntip(tree)) ; message('.', appendLF=FALSE)


#Testing tree.age
#example
tree_age<-tree.age(rtree(10), scale=1)

#test
#table
expect_is(tree_age, 'data.frame') ; message('.', appendLF=FALSE)
#min age is 0
expect_equal(min(tree_age[,1]), 0) ; message('.', appendLF=FALSE)
#max age is 1 (scale)
expect_equal(max(tree_age[,1]), 1) ; message('.', appendLF=FALSE)
