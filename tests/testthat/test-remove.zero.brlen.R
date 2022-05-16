#context("remove.zero.brlen")

## Test
test_that("remove.zero.brlen works", {
    ## Root connecting to a tip with zero branch length
    set.seed(1)
    my_tree <- rtree(5)
    my_tree$edge.length[1] <- 0

    error <- capture_error(remove.zero.brlen(my_tree))
    expect_equal(error[[1]], paste0("The root of the tree is connecting to a tip with a zero branch length: neither can be slid. You can try moving the tip manually by assigning a value to the following edge:\n    my_tree$edge.length[1] <- your_value"))

    ## No sliding
    set.seed(1)
    test <- remove.zero.brlen(rtree(5))
    expect_is(test, "phylo")
    expect_false(any(test$edge.length == 0))

    set.seed(42)
    ## Generating a tree
    tree <- rtree(20)
    ## Adding some zero branch lengths (5)
    tree$edge.length[sample(1:Nedge(tree), 5)] <- 0
    ## And now removing these zero branch lengths!
    tree_no_zero <- remove.zero.brlen(tree)
    ## Exaggerating the removal (to make it visible)
    tree_exaggerated <- remove.zero.brlen(tree, slide = 1)
    expect_is(tree, "phylo")
    expect_is(tree_no_zero, "phylo")
    expect_is(tree_exaggerated, "phylo")
    expect_true(any(tree$edge.length == 0))
    expect_false(any(tree_no_zero$edge.length == 0))
    expect_false(any(tree_exaggerated$edge.length == 0))

    ## Verbose version
    set.seed(1)
    message <- capture_output(remove.zero.brlen(tree, verbose = TRUE))
    expect_equal(message, "Changing 5 branch lengths:.....Done.")
})
