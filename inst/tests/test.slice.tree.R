#TEST SLICE.TREE
source("../../R/slice.tree.R")
source("../../R/slice.tree_fun.R")
source("../../R/tree.age.R")
source("../../R/tree.age_fun.R")
source("../../R/sanitizing.R")
library(testthat)
library(ape)
library(paleotree)

#Testing slice.tree_parent.node
#example
tree<-read.tree(text="(((((A:1,B:1):2,C:3):1,D:1):1,E:5):1,F:3);")
tree$node.label<-as.character(seq(1:5))

#test
#class
expect_error(slice.tree_parent.node(tree, '1')) ; message('.', appendLF=FALSE)
expect_is(slice.tree_parent.node(tree, 'B'), 'character') ; message('.', appendLF=FALSE)
#length
expect_error(length(slice.tree_parent.node(tree, '1'))) ; message('.', appendLF=FALSE)
expect_equal(length(slice.tree_parent.node(tree, 'B')), 1) ; message('.', appendLF=FALSE)
#null
expect_error(slice.tree_parent.node(rtree(5), '1')) ; message('.', appendLF=FALSE)
expect_error(slice.tree_parent.node(rtree(5), 'B')) ; message('.', appendLF=FALSE)
#sister nodes/taxa
expect_equal(slice.tree_parent.node(tree, 'A'), slice.tree_parent.node(tree, 'B')) ; message('.', appendLF=FALSE)
expect_equal(slice.tree_parent.node(tree, 'E'), slice.tree_parent.node(tree, '3')) ; message('.', appendLF=FALSE)


#Testing slice.tree_offspring.node
#example
tree<-read.tree(text="(((((A:1,B:1):2,C:3):1,D:1):1,E:5):1,F:3);")
tree$node.label<-as.character(seq(1:5))

#test
#error
expect_error(slice.tree_offspring.node(tree, '1')) ; message('.', appendLF=FALSE)
expect_error(slice.tree_offspring.node(tree, 'E', 'E')) ; message('.', appendLF=FALSE)
expect_error(slice.tree_offspring.node(tree, '5', 'E')) ; message('.', appendLF=FALSE)
expect_error(slice.tree_offspring.node(tree, '1', '2')) ; message('.', appendLF=FALSE)
#class
expect_is(slice.tree_offspring.node(tree, '1', 'A'), 'character') ; message('.', appendLF=FALSE)
#length
expect_equal(length(slice.tree_offspring.node(tree, '1', 'A')), 1) ; message('.', appendLF=FALSE)
#sister nodes/taxa
expect_equal(slice.tree_offspring.node(tree, '1', 'A'), slice.tree_offspring.node(tree, '1', 'E')) ; message('.', appendLF=FALSE)
expect_equal(slice.tree_offspring.node(tree, '4', 'A'), slice.tree_offspring.node(tree, '4', 'B')) ; message('.', appendLF=FALSE)


#Testing slice.tree_DELTRAN
#example
tree<-read.tree(text="(((((A:1,B:1):2,C:3):1,D:1):1,E:5):1,F:3);")
tree$node.label<-as.character(seq(1:5))
slice_tree<-suppressMessages(tree_slice<-timeSliceTree(tree, 3, drop.extinct=TRUE, plot=FALSE))
test<-slice.tree_DELTRAN(tree, 'A', tree_slice)

#test
#class
expect_is(test, 'character') ; message('.', appendLF=FALSE)
#length
expect_equal(length(test), 1) ; message('.', appendLF=FALSE)
#result (node 3)
expect_equal(test, '3') ; message('.', appendLF=FALSE)


#Testing ACCTRAN
#example
tree<-read.tree(text="(((((A:1,B:1):2,C:3):1,D:1):1,E:5):1,F:3);")
tree$node.label<-as.character(seq(1:5))
slice_tree<-suppressMessages(tree_slice<-timeSliceTree(tree, 3, drop.extinct=TRUE, plot=FALSE))
test<-slice.tree_ACCTRAN(tree, 'A', tree_slice)

#test
#class
expect_is(test, 'character') ; message('.', appendLF=FALSE)
#length
expect_equal(length(test), 1) ; message('.', appendLF=FALSE)
#result (node 4)
expect_equal(test, '4') ; message('.', appendLF=FALSE)


#Testing slice.tree
tree<-read.tree(text="(((((A:1,B:1):2,C:3):1,D:1):1,E:5):1,F:3);")
tree$node.label<-as.character(seq(1:5))

#test
#class
expect_is(slice.tree(tree, 0, 'ACCTRAN'), 'phylo') ; message('.', appendLF=FALSE)
#number of tips at time 0 (4)
expect_equal(Ntip(slice.tree(tree, 0, 'ACCTRAN')), 4) ; message('.', appendLF=FALSE)
#tips at time 0 ABCE
expect_equal(sort(slice.tree(tree, 0, 'ACCTRAN')$tip.label), LETTERS[c(1:3,5)]) ; message('.', appendLF=FALSE)
expect_equal(sort(slice.tree(tree, 0, 'DELTRAN')$tip.label), LETTERS[c(1:3,5)]) ; message('.', appendLF=FALSE)
#to old
expect_error(slice.tree(tree, 7, 'ACCTRAN')) ; message('.', appendLF=FALSE)
#to few taxa
expect_error(slice.tree(tree, 5.5, 'DELTRAN')) ; message('.', appendLF=FALSE)



#Complex slice.tree testing (requires diversitree - NOT RUN)
#require(diversitree)
#set.seed(1)
#tree<-tree.bd(c(1,0.5), max.taxa=20, max.t=Inf, include.extinct=TRUE)
#tree$node.label<-paste("n", seq(1:Nnode(tree)), sep="")
##expected results
#acc_0<-c("sp4","sp5","sp7","sp10","sp11","sp12","sp13","sp14","sp15","sp16","sp17","sp18","sp19","sp20","sp22","sp23","sp24","sp25","sp26","sp27")
#acc_05<-c("sp4","sp5","sp7","ex9","sp10","sp11","sp12","sp13","sp14","sp15","sp16","sp17","n24","sp20","n23")
#del_05<-c("n11","n13","n5","n8","n16","n19","n14","n20","n17","n17","n10","n21","n22","n18","n22")
#
##slice tests
#expect_equal(sort(slice.tree(tree, 0, 'ACCTRAN')$tip.label), sort(acc_0)) ; message('.', appendLF=FALSE)
#expect_equal(sort(slice.tree(tree, 0, 'DELTRAN')$tip.label), sort(acc_0)) ; message('.', appendLF=FALSE)
