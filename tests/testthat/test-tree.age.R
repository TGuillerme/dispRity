## TEST tree.age

context("tree.age")

## Testing tree.age_table
## example
tree <- rtree(10)
table_1 <- tree.age_table(tree)

## Test
test_that("tree.age_table works", {
    ## class
    expect_is(
    	table_1, 'data.frame'
    	)
    ## col.names
    expect_that(
    	colnames(table_1), equals(c('ages', 'elements'))
    	)
    ## row lengths
    expect_that(
    	nrow(table_1), equals(Ntip(tree) + Nnode(tree))
    	)
    ## ages are numeric
    expect_is(
    	table_1[,1], 'numeric'
    	)
    ## elements are factors
    suppressWarnings({same_names <- length(which(as.character(table_1[,2]) == as.character(tree$tip.label)))})
    expect_equal(
    	same_names, Ntip(tree)
    	)
})

## Test
test_that("tree.age_scale works", {
    ## class
    set.seed(1)
    tree <- rtree(10)
    tree_age <- tree.age(tree, order = 'present')
    tree_age_table <- tree.age_table(tree)

    test <- tree.age_scale(tree_age_table, scale = 10)
    expect_equal(test[3,1], 10)
    expect_equal(test[11,1], 0)
})

## Testing tree.age
## example
tree_age <- tree.age(rtree(10), age = 1)

## Test
test_that("tree.age works", {
    ## table
    expect_is(
    	tree_age, 'data.frame'
    	)
    ## min age is 0
    expect_equal(
    	min(tree_age[,1]), 0
    	)
    ## max age is 1 (age)
    expect_equal(
    	max(tree_age[,1]), 1
    	)
})

## Testing the example
test_that("Example runs", {
    ex1 <- tree.age(rtree(10), age = 50)
    expect_is(
    	ex1, "data.frame"
    	)
    expect_equal(
    	dim(ex1), c(19,2)
    	)
    ex2 <- tree.age(rtree(10), order = 'present')
    expect_is(
    	ex1, "data.frame"
    	)
    expect_equal(
    	dim(ex1), c(19,2)
    	)
})
