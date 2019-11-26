# TEST clean.data
context("clean.data")

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
    	cleaned[[3]], c("E", "F")
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


test_that("clean.data works with multiple tables and pairwise", {
    data1 <- matrix(c(rnorm(4), runif(4)), 4, 2, dimnames = list(LETTERS[1:4], c("var1", "var2")))
    data2 <- matrix(c(rnorm(5), runif(5)), 5, 2, dimnames = list(LETTERS[1:5], c("var1", "var2")))
    data_long <- list(data1, data2)
    tree1 <- rtree(4, tip.label = LETTERS[1:4])
    tree_long <- list(rtree(5, tip.label = LETTERS[1:5]), rtree(4, tip.label = LETTERS[1:4]))
    class(tree_long) <- "multiPhylo"

    test <- clean.data(data_long, tree_long)

    expect_is(test, "list")
    expect_equal(length(test$data), 2)
    expect_equal(length(test$tree), 2)
    expect_equal(test$dropped_tips, c("E"))
    expect_equal(test$dropped_rows, c("E"))

    test_pair <- clean.data(data_long, tree_long, 
        pairwise = TRUE)

    expect_is(test_pair, "list")
    expect_equal(length(test_pair), 2)
    expect_equal(length(test_pair[[1]]$data), 1)
    expect_equal(length(test_pair[[2]]$data), 1)
    expect_equal(length(test_pair[[1]]$tree), 1)
    expect_equal(length(test_pair[[2]]$tree), 1)
    expect_equal(test_pair[[1]]$dropped_tips, c("E"))
    expect_equal(test_pair[[1]]$dropped_rows, c(NA))
    expect_equal(test_pair[[2]]$dropped_tips, c(NA))
    expect_equal(test_pair[[2]]$dropped_rows, c("E"))

    error <- capture_error(clean.data(data2, tree_long, pairwise = TRUE))
    expect_equal(error[[1]], "Both data2 and tree_long must have the same length.")
})
