#TEST slice.tree

#context("slice.tree")


test_that("slice.tree.sharp works", {

    set.seed(42)
    tree <- rtree(10)
    tree$root.time <- 10

    ## Gives the same answers as timeSliceTree
    # for(test_slice in c(9,8,7)) {
    #     paletest <- paleotree::timeSliceTree(tree, test_slice, drop.extinct = TRUE, plot = FALSE)
    #     disptest <- slice.tree.sharp(tree, test_slice)
    #     expect_is(paletest, "phylo")
    #     expect_is(disptest, "phylo")
    #     expect_equal(paletest$tip.label, disptest$tip.label)
    #     expect_equal(dist.nodes(paletest), dist.nodes(disptest))
    # }
    # #microbenchmark(timeSliceTree(tree, 8, drop.extinct = TRUE, plot = FALSE), slicing.tree.sharp(tree, 8))

    # ## Provides error
    # expect_error(paleotree::timeSliceTree(tree, tree$root.time - (0.06 * tree$root.time), drop.extinct = TRUE, plot = FALSE))

    ## slice.tree.sharp doest not error
    expect_null(slice.tree.sharp(tree, tree$root.time - (0.06 * tree$root.time)))
})


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
    ## Nulls
    expect_null(
        slice.tree_offspring.node(tree, 'E', 'E')
        )
    expect_null(
        slice.tree_offspring.node(tree, '5', 'E')
        )
    expect_null(
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
# slice_tree <- suppressMessages(tree_slice <- paleotree::timeSliceTree(tree, 3, drop.extinct = TRUE, plot = FALSE))
tree_slice <- read.tree(text = "(((A, D), E), F);")
tree_slice$node.label <- as.character(c(1:3))
tree_slice$edge.length <- c(1, 1, 1, 1, 2, 3)
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


#Testing PROXIMITY
#example
test <- slice.tree_PROXIMITY(tree, 'A', tree_slice)

#Test
test_that("PROXIMITY picks up the right tip/node", {
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

test <- slice.tree_PROXIMITY(tree, 'A', tree_slice, probability = TRUE)

#Test
test_that("PROXIMITY probability right output", {
    #class
    expect_is(
        test, 'character'
        )
    #length
    expect_equal(
        length(test), 3
        )
    #result (node 4)
    expect_equal(
        test, c("3", "4", "0")
        )
})


#Testing slice.tree
tree <- read.tree(text = "(((((A:1,B:1):2,C:3):1,D:1):1,E:5):1,F:3);")
tree$node.label <- as.character(paste0("n",seq(1:5)))

#Test
test_that("slice.tree works properly", {
    set.seed(1)
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
        slice.tree(tree, 0, 'random'), 'phylo'
        )
    expect_is(
        slice.tree(tree, 0, 'randOM'), 'phylo'
        )
    expect_is(
        slice.tree(tree, 0, 'gradual.split'), 'matrix'
        )
    expect_is(
        slice.tree(tree, 0, 'equal.split'), 'matrix'
        )
    expect_equal(
        slice.edge(tree, 2, model = "random")
        ,"n2")
})

test_that("slice.tree proba works", {

    tree <- read.tree(text = "(((((A:1,B:1):2,C:3):1,D:1):1,E:5):1,F:3);")
    tree$node.label <- as.character(paste0("n",seq(1:5)))

    test0g <- slice.tree(tree, 0, "gradual.split")
    test0p <- slice.tree(tree, 0, "equal.split")

    LAD <- FAD <- tree.age(tree)

    LAD[10,1] <- LAD[4,1] <- 2.5
    FAD[10,1] <- FAD[4,1] <- 3.5

    ## Both have the same results
    expect_equal(test0g, test0p)

    ## Different results (no FADLAD)
    test0g_1 <- slice.tree(tree, 2.8, "gradual.split")
    test0p_1 <- slice.tree(tree, 2.8, "equal.split")
    expect_equal(round(as.numeric(test0g_1[,3]), digit = 3), c(0.900, 0.933, 0.560))
    expect_equal(round(as.numeric(test0p_1[,3]), digit = 3), c(0.500, 0.500, 0.500))

    ## Different results (with FADLAD)
    test0g_2 <- slice.tree(tree, 2.8, "gradual.split", FAD, LAD)
    test0p_2 <- slice.tree(tree, 2.8, "equal.split", FAD, LAD)
    expect_equal(round(as.numeric(test0g_2[,3]), digit = 3), c(1.000, 1.000, 0.560))
    expect_equal(round(as.numeric(test0p_2[,3]), digit = 3), c(1.000, 1.000, 0.500))

    ## Different results (with FAD or LAD)
    test0g_2 <- slice.tree(tree, 2.8, "gradual.split", FAD = FAD)
    test0p_2 <- slice.tree(tree, 2.8, "equal.split", LAD = LAD)
    expect_equal(round(as.numeric(test0g_2[,3]), digit = 3), c(0.9, 0.933, 0.560))
    expect_equal(round(as.numeric(test0p_2[,3]), digit = 3), c(1.000, 1.000, 0.500))
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


# Slice tree with single edge sliced
test_that("slice.tree works on a single edge", {

    tree <- read.tree(text = "((A:1, B:5):1, C:1);")
    tree$node.label <- as.character(paste0("n",seq(1:2)))
    expect_equal(slice.tree(tree, age = 1, model = "acctran"), "B")
    expect_equal(slice.tree(tree, age = 1, model = "acctran"), "B")
    expect_equal(slice.tree(tree, age = 1, "deltran"), "n2")
    expect_equal(slice.tree(tree, age = 1, "proximity"), "B")
    expect_equal(slice.tree(tree, age = 3.9, "proximity"), "n2")
    expect_equal(slice.tree(tree, age = 1, "equal.split"), c("n2", "B", "0.5"))
    expect_equal(slice.tree(tree, age = 3, "gradual.split"), c("n2", "B", "0.6"))
})


# Deep slice from example
test_that("example works with deep slice", {

    set.seed(1)
    ## Generate a random ultrametric tree
    tree <- rtree(20)
    ## Add some node labels
    tree$node.label <- letters[1:19]
    ## Add its root time
    tree$root.time <- max(tree.age(tree)$ages)
    ## Slice the tree at age 1.5
    tree_slice <- slice.tree(tree, age = 1.5, "deltran")
    ## The slice at age 0.5 but keeping all the ancestors
    deep_slice <- slice.tree(tree, age = 1.5, "deltran",
                                keep.all.ancestors = TRUE)

    expect_equal(deep_slice$tip.label, c("t10", "t14", "t20", "t7", "t9", "t15", "i", "l", "l", "o", "o", "t17"))
    expect_equal(which(round(tree.age(deep_slice, digits = 4)$ages, 1) == 1.5), 7:11)
    expect_equal(deep_slice$root.time, tree$root.time)
})