#TEST slice.tree

context("slice.tree")

#Testing slice.tree_parent.node
#example
tree <- read.tree(text = "(((((A:1,B:1):2,C:3):1,D:1):1,E:5):1,F:3);")
tree$node.label <- as.character(seq(1:5))

#Test
test_that("slice.tree_parent.node picks up the parent node", {
    #class
    expect_error(
        slice.tree_parent.node(tree, '1')
        )
    expect_is(
        slice.tree_parent.node(tree, 'B'), 'character'
        )
    #length
    expect_error(
        length(slice.tree_parent.node(tree, '1'))
        )
    expect_equal(
        length(slice.tree_parent.node(tree, 'B')), 1
        )
    #null
    expect_error(
        slice.tree_parent.node(rtree(5), '1')
        )
    expect_error(
        slice.tree_parent.node(rtree(5), 'B')
        )
    #sister nodes/taxa
    expect_equal(
        slice.tree_parent.node(tree, 'A'), slice.tree_parent.node(tree, 'B')
        )
    expect_equal(
        slice.tree_parent.node(tree, 'E'), slice.tree_parent.node(tree, '3')
        )
})

#Testing slice.tree_offspring.node

#Test
test_that("slice.tree_offspring.node picks up the offspring tip.node", {
    #error
    expect_error(
        slice.tree_offspring.node(tree, '1')
        )
    expect_error(
        slice.tree_offspring.node(tree, 'E', 'E')
        )
    expect_error(
        slice.tree_offspring.node(tree, '5', 'E')
        )
    expect_error(
        slice.tree_offspring.node(tree, '1', '2')
        )
    #class
    expect_is(
        slice.tree_offspring.node(tree, '1', 'A'), 'character'
        )
    #length
    expect_equal(
        length(slice.tree_offspring.node(tree, '1', 'A')), 1
        )
    #sister nodes/taxa
    expect_equal(
        slice.tree_offspring.node(tree, '1', 'A'), slice.tree_offspring.node(tree, '1', 'E')
        )
    expect_equal(
        slice.tree_offspring.node(tree, '4', 'A'), slice.tree_offspring.node(tree, '4', 'B')
        )
})

#Testing slice.tree_DELTRAN
#example
tree <- read.tree(text = "(((((A:1,B:1):2,C:3):1,D:1):1,E:5):1,F:3);")
tree$node.label <- as.character(seq(1:5))
slice_tree <- suppressMessages(tree_slice <- paleotree::timeSliceTree(tree, 3, drop.extinct = TRUE, plot = FALSE))
test <- slice.tree_DELTRAN(tree, 'A', tree_slice)

#Test
test_that("DELTRAN picks up the parent node", {
    #class
    expect_is(
        test, 'character'
        )
    #length
    expect_equal(
        length(test), 1
        )
    #result (node 3)
    expect_equal(
        test, '3'
        )
})

#Testing ACCTRAN
test <- slice.tree_ACCTRAN(tree, 'A', tree_slice)
#Test
test_that("ACCTRAN picks up the offspring tip/node", {
    #class
    expect_is(
        test, 'character'
        )
    #length
    expect_equal(
        length(test), 1
        )
    #result (node 4)
    expect_equal(
        test, '4'
        )
})


#Testing GRADUAL
#example
test <- slice.tree_GRADUAL(tree, 'A', tree_slice)

#Test
test_that("GRADUAL picks up the right tip/node", {
    #class
    expect_is(
        test, 'character'
        )
    #length
    expect_equal(
        length(test), 1
        )
    #result (node 4)
    expect_equal(
        test, '4'
        )
})


#Testing slice.tree
tree <- read.tree(text = "(((((A:1,B:1):2,C:3):1,D:1):1,E:5):1,F:3);")
tree$node.label <- as.character(seq(1:5))

#Test
test_that("slice.tree works properly", {
    #class
    expect_is(
        slice.tree(tree, 0, 'ACCTRAN'), 'phylo'
        )
    #number of tips at time 0 (4)
    expect_equal(
        Ntip(slice.tree(tree, 0, 'ACCTRAN')), 4
        )
    #tips at time 0 ABCE
    expect_equal(
        sort(slice.tree(tree, 0, 'ACCTRAN')$tip.label), LETTERS[c(1:3,5)]
        )
    expect_equal(
        sort(slice.tree(tree, 0, 'DELTRAN')$tip.label), LETTERS[c(1:3,5)]
        )
    expect_is(
        slice.tree(tree, 0, 'proximity'), 'phylo'
        )
    expect_is(
        slice.tree(tree, 0, 'punCTUAted'), 'phylo'
        )
})


#Complex slice.tree testing (requires diversitree - NOT RUN)
#require(diversitree)
#set.seed(1)
#tree <- tree.bd(c(1,0.5), max.taxa = 20, max.t = Inf, include.extinct = TRUE)
#tree$node.label <- paste("n", seq(1:Nnode(tree)), sep = "")
##expected results
#acc_0 <- c("sp4","sp5","sp7","sp10","sp11","sp12","sp13","sp14","sp15","sp16","sp17","sp18","sp19","sp20","sp22","sp23","sp24","sp25","sp26","sp27")
#acc_05 <- c("sp4","sp5","sp7","ex9","sp10","sp11","sp12","sp13","sp14","sp15","sp16","sp17","n24","sp20","n23")
#del_05 <- c("n11","n13","n5","n8","n16","n19","n14","n20","n17","n17","n10","n21","n22","n18","n22")
#
##slice tests
#expect_equal(
#        sort(slice.tree(tree, 0, 'ACCTRAN')$tip.label), sort(acc_0)
#        )
#expect_equal(
#        sort(slice.tree(tree, 0, 'DELTRAN')$tip.label), sort(acc_0)
#        )
