# TEST clean.data
#context("clean.data")

# Testing clean.tree.table
tree <- rtree(6, tip.label = LETTERS[1:6])
data <- matrix(data =c(rnorm(4), runif(4)), 4, 2, dimnames = list(LETTERS[2:5]))
test <- clean.tree.table(tree, data)
test_that("clean.tree.table works", {
    # Errors
    expect_error(
    	clean.tree.table(TRUE)
    	)
    # Output is a list...
    expect_is(
    	test, "list"
    	)
    # ... of 4 elements.
    expect_equal(
    	length(test), 4
    	)
    # First element is a tree...
    expect_is(
    	test[[1]], "phylo"
    	)
    # ...with three taxa.
    expect_equal(
    	Ntip(test[[1]]), 4
    	)
    # Second element is a table...
    expect_is(
    	test[[2]], "matrix"
    	)
    # ...with three rows.
    expect_equal(
    	nrow(test[[2]]), 4
    	)
    # Third element contains "F" and "A"
    expect_equal(
    	sort(test[[3]]), c("A", "F")
    	)
    # Forth element contains NA
    expect_equal(
    	test[[4]], NA
    	)
})

#Testing clean.data
trees_list <- list(rtree(5, tip.label = LETTERS[1:5]), rtree(4, tip.label = LETTERS[1:4]), rtree(6, tip.label = LETTERS[1:6])) ; class(trees_list) <- "multiPhylo"
dummy_data <- matrix(c(rnorm(5), runif(5)), 5, 2, dimnames = list(LETTERS[1:5], c("var1", "var2")))
cleaned <- clean.data(data = dummy_data, tree = trees_list)
test_that("clean.data works", {
    
    ## Errors
    data_test <- dummy_data
    rownames(data_test) <- NULL
    expect_error(
        clean.data(data_test, tree_list)
        )

    # Output is a list...
    expect_is(
    	cleaned, "list"
    	)
    # ... of 4 elements.
    expect_equal(
    	length(cleaned), 4
    	)
    # First element are trees...
    expect_is(
    	cleaned[[1]], "multiPhylo"
    	)
    # ...with 4 taxa each.
    expect_equal(
    	unique(unlist(lapply(cleaned[[1]], Ntip))), 4
    	)
    # Second element is a table...
    expect_is(
    	cleaned[[2]], "matrix"
    	)
    # ...with four rows.
    expect_equal(
    	nrow(cleaned[[2]]), 4
    	)
    # Third element contains "F"
    expect_equal(
    	cleaned[[3]], "F"
    	)
    # Forth element contains "E"
    expect_equal(
    	cleaned[[4]], "E"
    	)

    ## Working with a single tree
    test <- clean.data(dummy_data, trees_list[[1]])
    expect_equal(
        names(test)
        , c("tree", "data", "dropped_tips", "dropped_rows")
        )
    expect_true(
        is.na(test[[3]])
        )
    expect_true(
        is.na(test[[4]])
        )

    ## Tree is OK
    tree <- rtree(5, tip.label = LETTERS[1:5])
    multi_tree <- rmtree(5, 5)
    multi_tree <- lapply(multi_tree, function(X) {X$tip.label <- LETTERS[1:5]; return(X)})
    class(multi_tree) <- "multiPhylo"
    dummy_data <- matrix(c(rnorm(6), runif(6)), 6, 2, dimnames = list(LETTERS[1:6], c("var1", "var2")))
    expect_equal(clean.data(dummy_data, tree)$dropped_tips, NA)
    expect_equal(clean.data(dummy_data, multi_tree)$dropped_tips, NA)
    
    ## Data is OK
    tree <- rtree(6, tip.label = LETTERS[1:6])
    multi_tree <- rmtree(5, 6)
    multi_tree <- lapply(multi_tree, function(X) {X$tip.label <- LETTERS[1:6]; return(X)})
    class(multi_tree) <- "multiPhylo"
    dummy_data <- matrix(c(rnorm(5), runif(5)), 5, 2, dimnames = list(LETTERS[1:5], c("var1", "var2")))
    expect_equal(clean.data(dummy_data, tree)$dropped_rows, NA)
    expect_equal(clean.data(dummy_data, multi_tree)$dropped_rows, NA)
})

## Clean.data with nodes
test_that("clean.data works with nodes", {
    tree0 <- rtree(5)
    tree1 <- makeNodeLabel(rtree(5))
    tree010 <- list(tree0, tree1, tree0) ; class(tree010) <- "multiPhylo"
    tree2 <- makeNodeLabel(rtree(5))
    tree2$node.label[1] <- "bob"
    tree12 <- list(tree1, tree2) ; class(tree12) <- "multiPhylo"
    data <- matrix(0, nrow = 9, ncol = 2, dimnames = list(c(paste0("t", 1:5), paste0("Node", 1:4))))

    ## Test nulls
    test <- clean.data(data, tree0, inc.nodes = FALSE)
    expect_equal(test$dropped_rows, paste0("Node", 1:4))
    error <- capture_error(clean.data(data, tree0, inc.nodes = TRUE))
    expect_equal(error[[1]], "Impossible to use check the nodes in the tree because the tree has no nodes. Set the option inc.nodes = FALSE, or add node labels to the tree (e.g. using ape::makeNodeLabels(...)).")
    error <- capture_error(clean.data(data, tree010, inc.nodes = TRUE))
    expect_equal(error[[1]], "Impossible to use check the nodes in the trees 1, 3 because the tree has no nodes. Set the option inc.nodes = FALSE, or add node labels to the tree (e.g. using ape::makeNodeLabels(...)).")

    ## All good
    test <- clean.data(data, tree1, inc.nodes = TRUE)
    expect_true(is.na(test$dropped_tips))
    expect_true(is.na(test$dropped_rows))

    ## Error! no node match
    error <- capture_error(clean.data(data, tree2, inc.nodes = TRUE))
    expect_equal(error[[1]], "Node bob not found in the data (nodes cannot be trimmed automatically).")

    ## Error in the 2nd tree
    error <- capture_error(clean.data(data, tree12, inc.nodes = TRUE))
    expect_equal(error[[1]], "Node bob from tree 2 not found in the data. (nodes cannot be trimmed automatically).")

})

