## TESTING custom.subsets

#context("custom.subsets")

data <- matrix(data = rnorm(10*9), nrow = 10, ncol = 9)
rownames(data) <- letters[1:10]

## Internal functions
test_that("check.elements.data.frame", {
    ## Returns false if all groups have > 3 elements
    expect_false(check.elements.data.frame(c(rep(1,3), rep(2,4))))
    ## else returns true
    expect_true(check.elements.data.frame(c(rep(1,2), rep(2,4))))
})

test_that("convert.name.to.numbers", {
    ## Returns the matching rownames ...
    expect_equal(convert.name.to.numbers(c("a", "b"), data), c(1,2))
    ## ... in the right input order
    expect_equal(convert.name.to.numbers(c("d", "a"), data), c(4,1))
    ## returns NA if no match
    expect_true(is.na(convert.name.to.numbers(c("X"), data)))
})

test_that("split.elements.data.frame", {
    test <- split.elements.data.frame(c(rep(1,5), rep(2,5)), data)

    ## Must be a list of two elements ("1" and "2") with a list of 5 elements each within.
    expect_is(test, "list")
    expect_equal(length(test), 2)
    expect_equal(names(test), c("1", "2"))
    expect_equal(as.vector(unlist(lapply(test, lapply, length))), c(5,5))

})

test_that("set.group.list works", {

    set.seed(41)
    data <- matrix(data = rnorm(90), nrow = 9, ncol = 10, dimnames = list(letters[1:9]))
    group1 <- list("A" = c(1,2,3,4), "B" = c(5,6,7,8,9))
    group2 <- list("A" = c("a", "b", "c", "d"), "B" = c(letters[5:9]))
    group3 <- as.data.frame(matrix(data = c(rep(1,5), rep(2,4)), nrow = 9, ncol = 1, dimnames = list(letters[1:9])))
    group4 <- matrix(data = c(rep(1,5), rep(2,4)), nrow = 9, ncol = 1, dimnames = list(letters[1:9]))
    group5 <- rtree(10, tip.label = letters[1:10])
    group6 <- rtree(5, tip.label = letters[1:5])
    group6$node.label <- letters[6:9]

    ## List numbers
    test <- set.group.list(group1, data)
    expect_is(test, "list")
    expect_equal(length(test), 2)
    expect_equal(unlist(lapply(test, length)), c("A" = 4, "B" = 5))
    ## List letters
    test <- set.group.list(group2, data)
    expect_is(test, "list")
    expect_equal(length(test), 2)
    expect_equal(unlist(lapply(test, length)), c("A" = 4, "B" = 5))
    ## data frame
    test <- set.group.list(group3, data)
    expect_is(test, "list")
    expect_equal(length(test), 2)
    expect_equal(unlist(lapply(test, length)), c("1" = 5, "2" = 4))
    ## matrix
    test <- set.group.list(group4, data)
    expect_is(test, "list")
    expect_equal(length(test), 2)
    expect_equal(unlist(lapply(test, length)), c("1" = 5, "2" = 4))
    ## tree
    test <- set.group.list(group5, data)
    expect_is(test, "list")
    expect_equal(length(test), 9)
    expect_equal(unlist(lapply(test, length)), c(10, 7, 3, 2, 4, 3, 2, 3, 2))
    ## tree nodes
    test <- set.group.list(group6, data)
    expect_is(test, "list")
    expect_equal(length(test), 4)
    expect_equal(unlist(lapply(test, length)), c(9, 3, 5, 3))
})


## Sanitizing
test_that("Sanitizing works", {
    ## class
    expect_error(
        custom.subsets(data, group = "A")
        )
    ## same number of rows
    group <- matrix(5,5)
    expect_error(
        custom.subsets(data, group)
        )
    group <- as.data.frame(matrix(data = c(rep(1,5), rep(2,5)), nrow = 10, ncol = 1))
    expect_error(
        custom.subsets(data, group)
        )
    ## row names must be the same
    rownames(group) <- letters[2:11]
    expect_error(
        custom.subsets(data, group)
        )
    ## One class with only 3 elements
    rownames(group) <- letters[1:10]
    group[1:2,1] <- 3
    expect_error(
        custom.subsets(data, group)
        )
    ## Wrong tree
    expect_error(
        custom.subsets(data, rtree(10))
    )
    tree <- rtree(9)
    tree$tip.label <- letters[1:9]
    tree$node.label <- NULL
    expect_error(
        custom.subsets(data, tree)
    )
    tree <- rtree(5)
    tree$tip.label <- letters[1:5]
    tree$node.label <- letters[7:10]
    expect_error(
        custom.subsets(data[1:9,], tree)
    )
    tree <- rtree(9)
    tree$tip.label <- letters[1:9]
    data_wrong <- data
    rownames(data_wrong)[1] <- "AAA"
    error <- capture_error(custom.subsets(data_wrong, tree))
    expect_equal(error[[1]], "Some tips in the tree are not matching the data.\nSee ?clean.data for matching the tree and the data.")

    ## Wrong names as list
    error <- capture_error(custom.subsets(data, group = list(letters[1:5], letters[20:25])))
    expect_equal(error[[1]], "Row names in data and group arguments don't match.")    
    error <- capture_error(custom.subsets(data, group = list(1:5, 20:25)))
    expect_equal(error[[1]], "Row numbers in group don't match the row numbers in data.")
})

## Results
group <- as.data.frame(matrix(data = c(rep(1,5), rep(2,5)), nrow = 10, ncol = 1))
rownames(group) <- letters[1:10]
test <- custom.subsets(data, group)

## Test
test_that("custom.subsets works", {
    expect_is(
        test
        , "dispRity")
    expect_equal(
        length(test)
        , 4)
    expect_is(
        test$matrix[[1]]
        , "matrix")
    expect_equal(
        dim(test$matrix[[1]])
        , c(10,9))
    expect_equal(
        length(test$subsets[[1]]$elements)
        ,5)
    expect_equal(
        length(test$subsets[[2]]$elements)
        ,5)
})

## Test
test_that("Different group inputs gives the same output", {

    group1 <- list("A" = c(1,2,3,4), "B" = c(5,6,7,8,9,10))
    group2 <- list("A" = c("a", "b", "c", "d"), "B" = c(letters[5:10]))
    group3 <- as.data.frame(matrix(data = c(rep(1,4), rep(2,6)), nrow = 10, ncol = 1, dimnames = list(letters[1:10])))

    cust1 <- custom.subsets(data, group1)
    cust2 <- custom.subsets(data, group2)
    cust3 <- custom.subsets(data, group3)

    expect_equal(
        unique(unlist(lapply(list(cust1, cust2, cust3), class)))
        , "dispRity")
    expect_equal(
        unique(unlist(lapply(list(cust1, cust2, cust3), length)))
        , 4)

    expect_true(
        all(as.vector(cust1$subsets[[1]]$elements) == as.vector(cust2$subsets[[1]]$elements))
    )
    expect_true(
        all(as.vector(cust1$subsets[[1]]$elements) == as.vector(cust3$subsets[[1]]$elements))
    )
    expect_true(
        all(as.vector(cust2$subsets[[1]]$elements) == as.vector(cust3$subsets[[1]]$elements))
    )
    expect_true(
        all(as.vector(cust1$subsets[[2]]$elements) == as.vector(cust2$subsets[[2]]$elements))
    )
    expect_true(
        all(as.vector(cust1$subsets[[2]]$elements) == as.vector(cust3$subsets[[2]]$elements))
    )
    expect_true(
        all(as.vector(cust2$subsets[[2]]$elements) == as.vector(cust3$subsets[[2]]$elements))
    )
})


## Example
test_that("Example works", {

    ordinated_matrix <- matrix(data = rnorm(90), nrow = 10, ncol = 9, dimnames = list(letters[1:10]))

    expect_error(
        custom.subsets(ordinated_matrix, list("A" = c("a", "b", "c", "d"), "B" = c("e", "f", "g", "h", "i", "j"), "C" = sum))
        )

    ## Splitting the ordinated matrix into two groups using row numbers
    numbers <- custom.subsets(ordinated_matrix, list("A" = c(1:4), "B" = c(5:10)))
    ## Splitting the ordinated matrix into three groups using row names
    Letters <- custom.subsets(ordinated_matrix, list("A" = c("a", "b", "c", "d"), "B" = c("e", "f", "g", "h", "i", "j"), "C" = c("a", "c", "d", "f", "h")))
    ## Splitting the ordinated matrix into four groups using a data frame
    groups <- as.data.frame(matrix(data = c(rep(1,5), rep(2,5), rep(c(1,2), 5)), nrow = 10, ncol = 2, dimnames = list(letters[1:10], c("g1", "g2"))))
    dataframe <- custom.subsets(ordinated_matrix, groups)

    expect_is(numbers, "dispRity")
    expect_is(Letters, "dispRity")
    expect_is(dataframe, "dispRity")

    expect_equal(
        as.vector(unlist(lapply(numbers$subsets, lapply, length)))
        , c(4,6))
    expect_equal(
        names(unlist(lapply(numbers$subsets, lapply, length)))
        , c("A.elements", "B.elements"))

    expect_equal(
        as.vector(unlist(lapply(Letters$subsets, lapply, length)))
        , c(4,6,5))
    expect_equal(
        names(unlist(lapply(Letters$subsets, lapply, length)))
        , c("A.elements", "B.elements", "C.elements"))
    expect_equal(
        as.vector(unlist(lapply(dataframe$subsets, lapply, length)))
        , c(rep(5,4)))
    expect_equal(
        names(unlist(lapply(dataframe$subsets, lapply, length)))
        , c("g1.1.elements", "g1.2.elements", "g2.1.elements", "g2.2.elements"))
})

## Subsample works with an empty element
test_that("empty custom.subsets", {
    data <- matrix(data = rnorm(90), nrow = 10, ncol = 9, dimnames = list(letters[1:10]))
    group4 <- list("A" = NULL, "B" = c(1,2), "C" = c(3,4,5), "D" = 1, "E" = NA)
    group5 <- list("B" = c(1,2), "C" = c(3,4,5), "D" = 1, "E" = NA)

    warning <- capture_warnings(test <- custom.subsets(data, group4))

    expect_equal(warning, "Subsamples A, E are empty.")
    expect_is(test, "dispRity")
    expect_equal(length(test$subsets), 5)

    expect_equal(capture_warnings(custom.subsets(data, group5)), "Subsample E is empty.")
}) 

## Subsets works with a tree
test_that("clade subsets works", {
    data(BeckLee_mat50)
    data(BeckLee_mat99)
    data(BeckLee_tree)

    without_nodes <- custom.subsets(BeckLee_mat50, group = BeckLee_tree)
    with_nodes <- custom.subsets(BeckLee_mat99, group = BeckLee_tree)

    ## Both contain the same number of groups (Nnodes)
    expect_equal(length(without_nodes$subsets), Nnode(BeckLee_tree))
    expect_equal(length(with_nodes$subsets), Nnode(BeckLee_tree))

    ## Both first groups contain all the data (root)
    expect_equal(nrow(without_nodes$subsets[[1]]$elements), nrow(BeckLee_mat50))
    expect_equal(nrow(with_nodes$subsets[[1]]$elements), nrow(BeckLee_mat99))

    ## Expect the trees are present
    expect_is(without_nodes$tree[[1]], "phylo")
    expect_is(with_nodes$tree[[1]], "phylo")
})



test_that("custom.subsets detects distance matrices", {
    non_dist <- matrix(1:100, 10, 10)
    rownames(non_dist) <- letters[1:10]
    is_dist <- as.matrix(dist(non_dist))

    expect_warning(custom.subsets(is_dist, group = list(letters[1:5], letters[6:10])))
    msg <- capture_warnings(custom.subsets(is_dist, group = list(letters[1:5], letters[6:10])))
    expect_equal(msg, "custom.subsets is applied on what seems to be a distance matrix.\nThe resulting matrices won't be distance matrices anymore!")
})


test_that("custom.subsets works with tree", {
    data(BeckLee_mat50)
    data(BeckLee_mat99)
    data(BeckLee_tree)
    test <- custom.subsets(data = BeckLee_mat50, group = list(c(1:5), c(5,7)), tree = BeckLee_tree)
    expect_is(test$tree[[1]], "phylo")
    expect_equal(length(test$tree), 1)
    test <- custom.subsets(data = BeckLee_mat99, group = list(c(1:5), c(5,7)), tree = BeckLee_tree)
    expect_is(test$tree[[1]], "phylo")
    expect_equal(length(test$tree), 1)
    test <- custom.subsets(data = BeckLee_mat99, group = list(c(1:5), c(5,7)), tree = c(BeckLee_tree, BeckLee_tree, BeckLee_tree))
    expect_is(test$tree[[1]], "phylo")
    expect_equal(length(test$tree), 3)
})