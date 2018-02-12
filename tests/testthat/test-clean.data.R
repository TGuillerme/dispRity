# TEST clean.data
context("clean.data")

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
})

