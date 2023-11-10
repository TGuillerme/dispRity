## TEST sanitizing

#context("sanitizing")

## Testing class.check
## examples
class_1<-NA ; class(class_1) <- 'class_1'
class_2<-NA ; class(class_2) <- 'class_2'
class_3<-NA ; class(class_3) <- 'class_3'
class_list<-c("class_1", "class_2")

## Test
test_that("check.class works", {
    ## class - single
    expect_null(
            check.class(class_1, "class_1", 'test')
            )
    expect_error(
            check.class(class_1, "class_1", 'test', errorif=TRUE)
            )
    expect_null(
            check.class(class_2, "class_2", 'test')
            )
    expect_error(
            check.class(class_2, "class_2", 'test', errorif=TRUE)
            )
    ## class - multiple
    expect_that(
            check.class(class_1, class_list, 'test'), equals("class_1")
            )
    expect_that(
            check.class(class_2, class_list, 'test'), equals("class_2")
            )
    expect_error(
            check.class(class_3, class_list, 'test')
            )

    expect_that(
            check.class(class_2, class_list, 'test', errorif = TRUE), equals("class_2"))
})

## Check class function
test_that("check.class works with functions", {
    object_fun <- function(x) return(x+1)
    object_std <- mean

    ## Normal behavior
    expect_is(
        object_fun
        ,"function")
    expect_null(
        check.class(object_fun, "function")
    )
    expect_null(
        check.class(object_std, "function")
    )
    expect_error(
        check.class(object_std, "numeric")
    )
    expect_error(
        check.class(object_fun, "standardGeneric")
    )
})

## Testing check.length
## examples
length_1 <- 1
length_2 <- c(1, 1)
length_3 <- NA
length_4 <- "1"

## Test
test_that("check.length works", {
    expect_null(
            check.length(length_1, '1', 'test')
            )
    expect_null(
            check.length(length_3, '1', 'test')
            )
    expect_null(
            check.length(length_4, '1', 'test')
            )
    expect_error(
            check.length(length_2, '1', 'test')
            )
    expect_error(
            check.length(length_1, '1', 'test', errorif=TRUE)
            )
})

## Test methods
test_that("check.method works", {
    methods <- letters[1:2]
    expect_null(check.method("a", methods))
    expect_null(check.method("b", methods))
    expect_null(check.method(c("a", "b"), methods))
    expect_null(check.method(c("b", "a"), methods))
    expect_error(check.method("c", methods))
    expect_null(check.method(c("a", "c"), methods))
})

## Test distance
test_that("check.dist.matrix works", {
    non_dist <- matrix(1:9, 3, 3)
    is_dist <- as.matrix(dist(non_dist))

    test <- check.dist.matrix(dist(non_dist))
    expect_equal(test[[1]], dist(non_dist))
    expect_true(test$was_dist)

    expect_error(check.dist.matrix(non_dist, just.check = "blabla"))
    expect_error(check.dist.matrix(non_dist, just.check = FALSE))
    expect_false(check.dist.matrix(non_dist, just.check = TRUE))
    expect_true(check.dist.matrix(is_dist, just.check = TRUE))
    test <- check.dist.matrix(non_dist, just.check = FALSE, method = "euclidean")
    expect_equal(class(test), "list")
    expect_equal(class(test[[1]]), "dist")
    expect_false(test[[2]])
    test <- check.dist.matrix(is_dist, just.check = FALSE, method = "euclidean")
    expect_equal(class(test), "list")
    expect_equal(class(test[[1]]), "dist")
    expect_true(test[[2]])
    expect_true(all(test[[1]] == dist(non_dist)))
})

## Stop call
test_that("stop.call works", {

    return.match.call <- function(character, numeric, fun, formula, matrix) {
        match_call <- match.call()
        return(match_call)
    }

    my_matrix <- matrix(NA)
    call <- return.match.call("a", 1, mean, ~ a + b, my_matrix)

    test <- capture_error(stop.call(call$character, " works."))
    expect_equal(test[[1]], "a works.")
    test <- capture_error(stop.call(call$numeric, " works."))
    expect_equal(test[[1]], "1 works.")
    test <- capture_error(stop.call(call$fun, " works."))
    expect_equal(test[[1]], "mean works.")
    test <- capture_error(stop.call(call$formula, " works."))
    expect_equal(test[[1]], "~a + b works.")
    test <- capture_error(stop.call(call$matrix, " works."))
    expect_equal(test[[1]], "my_matrix works.")

    test <- capture_error(stop.call(call$character, " works.", "look: "))
    expect_equal(test[[1]], "look: a works.")
})

## Check class
test_that("check.class works", {
    list <- list("a" = "a", "1" = 1, "tree" = rtree(5))
    test <- check.list(list, function(x) !is.null(x))
    expect_equal(test, c("a" = TRUE, "1" = TRUE, "tree" = TRUE))

    test <- check.list(list, is.null, condition = any)
    expect_equal(test, c("a" = FALSE, "1" = FALSE, "tree" = FALSE))

    test <- check.list(list, function(x) (x == "a"), condition = any)
    expect_equal(test, c("a" = TRUE, "1" = FALSE, "tree" = FALSE))    
})

## Test round
test_that("test_equal_round works", {
    x <- 1.111111
    y <- 1.11
    expect_equal(expect_equal_round(x, y, digits = 2), 1.11)
})

## Test check.data
test_that("check.data works", {

    match_call <- list("data" = "my_data")

    ## All errors
    error <- capture_error(check.data("a", match_call)$matrix)
    expect_equal(error[[1]], "data must be of class matrix or data.frame or list.")
    error <- capture_error(check.data(list(matrix(c(1,2)), "a"), match_call)$matrix)
    expect_equal(error[[1]], "my_data must be matrix or a list of matrices with the same dimensions and unique row names.")
    
    ## Matrix input
    bob <- matrix(c(1,2))
    warn <- capture_warnings(test <- check.data(bob, match_call)$matrix)
    expect_equal(warn[[1]], "Row names have been automatically added to my_data.")
    expect_is(test, "list")
    expect_is(test[[1]], "matrix")
    expect_equal(dim(test[[1]]), c(2,1))
    expect_equal(rownames(test[[1]]), c("1","2"))

    bob <- matrix(c(1,2))
    rownames(bob) <- c(1,2)
    test <- check.data(bob, match_call)$matrix
    expect_is(test, "list")
    expect_is(test[[1]], "matrix")
    expect_equal(dim(test[[1]]), c(2,1))
    expect_equal(rownames(test[[1]]), c("1","2"))

    ## List input
    bib <- list(matrix(c(1,2)))
    warn <- capture_warnings(test <- check.data(bib, match_call)$matrix)
    expect_equal(warn[[1]], "Row names have been automatically added to my_data.")
    expect_is(test, "list")
    expect_is(test[[1]], "matrix")
    expect_equal(dim(test[[1]]), c(2,1))
    expect_equal(rownames(test[[1]]), c("1","2"))

    bob <- list(matrix(c(1,2)), matrix(c(1,2)))
    warn <- capture_warnings(test <- check.data(bob, match_call)$matrix)
    expect_equal(warn[[1]], "Row names have been automatically added to my_data.")
    expect_is(test, "list")
    expect_is(test[[1]], "matrix")
    expect_equal(dim(test[[1]]), c(2,1))
    expect_equal(dim(test[[2]]), c(2,1))
    expect_equal(rownames(test[[1]]), c("1","2"))
    expect_equal(rownames(test[[2]]), c("1","2"))

    bub <- list(matrix(c(1,2), dimnames = list(c(1:2), 1)), matrix(c(1,2), dimnames = list(c(2:1), 1)))
    test <- check.data(bub, match_call)$matrix
    expect_is(test, "list")
    expect_is(test[[1]], "matrix")
    expect_equal(dim(test[[1]]), c(2,1))
    expect_equal(dim(test[[2]]), c(2,1))
    expect_equal(rownames(test[[1]]), c("1","2"))
    expect_equal(rownames(test[[2]]), c("1","2"))

    ## Different matrices
    ## error (not the same number of rows)
    data <- list(matrix(1, nrow = 2, ncol = 1, dimnames = list(letters[1:2])), matrix(1, nrow = 2, ncol = 2, dimnames = list(letters[1:2])))
    error <- capture_error(check.data(data, match_call))
    expect_equal(error[[1]], "my_data must be matrix or a list of matrices with the same dimensions and unique row names.")
    ## works (outputs multi = FALSE)
    data <- list(matrix(1, nrow = 2, ncol = 1, dimnames = list(letters[1:2])), matrix(1, nrow = 2, ncol = 1, dimnames = list(letters[1:2])))
    test <- check.data(data, match_call)
    expect_is(test, "list")
    expect_equal(names(test), c("matrix", "multi"))
    expect_false(test$multi)
    ## works (outputs multi = TRUE) # warning: differing stuff
    data <- list(matrix(1, nrow = 2, ncol = 1, dimnames = list(letters[1:2])), matrix(1, nrow = 2, ncol = 1, dimnames = list(letters[2:3])))
    warn <- capture_warning(check.data(data, match_call))
    expect_equal(warn[[1]], "The following elements are not present in all matrices: c, a. The matrices will be treated as separate trait-spaces.")
    expect_warning(test <- check.data(data, match_call))
    expect_true(test$multi)
    ## works (outputs multi = TRUE) # warning: differing stuff
    data <- list(matrix(1, nrow = 2, ncol = 1, dimnames = list(letters[1:2])), matrix(1, nrow = 3, ncol = 1, dimnames = list(letters[1:3])))
    warn <- capture_warning(check.data(data, match_call))
    expect_equal(warn[[1]], "The following elements are not present in all matrices: c. The matrices will be treated as separate trait-spaces.")
    expect_warning(test <- check.data(data, match_call))
    expect_true(test$multi)
})

## Test check.tree
test_that("check.tree works", {
    set.seed(1)
    match_call <- list(tree = "my_tree", data = "my_data")
    ## One tree one data
    tree <- makeNodeLabel(rtree(5))
    data <- matrix(0, nrow = 9, ncol = 2, dimnames = list(c(paste0("t", 1:5), paste0("Node", 1:4))))
    data <- fill.dispRity(make.dispRity(data = check.data(data, match_call)$matrix))

    ## Basic error
    error <- capture_error(check.tree(tree = "tree", data = data, bind.trees = FALSE, match_call))
    expect_equal(error[[1]], "tree must be of class phylo or multiPhylo.")
    ## Basic works
    test <- check.tree(tree = tree, data = data, bind.trees = FALSE, match_call)$tree
    expect_is(test, "multiPhylo")
    expect_is(test[[1]], "phylo")
    expect_equal(length(test), 1)

    ## Multiple tree one data
    tree <- makeNodeLabel(rtree(5))
    tree <- list(tree, tree)
    class(tree) <- "multiPhylo"
    data <- matrix(0, nrow = 9, ncol = 2, dimnames = list(c(paste0("t", 1:5), paste0("Node", 1:4))))
    data <- fill.dispRity(make.dispRity(data = check.data(data, match_call)$matrix))

    ## Not all trees have node labels
    tree_error <- tree
    tree_error[[1]]$node.label <- NULL
    error <- capture_error(check.tree(tree = tree_error, data = data, bind.trees = FALSE, match_call))
    expect_equal(error[[1]], "All trees should have node labels or no node labels.")
    ## multiple trees works
    test <- check.tree(tree = tree, data = data, bind.trees = FALSE, match_call)$tree
    expect_is(test, "multiPhylo")
    expect_is(test[[1]], "phylo")
    expect_equal(length(test), 2)

    ## One tree multiple data
    tree <- makeNodeLabel(rtree(5))
    data <- matrix(0, nrow = 9, ncol = 2, dimnames = list(c(paste0("t", 1:5), paste0("Node", 1:4))))
    data <- fill.dispRity(make.dispRity(data = check.data(list(data, data), match_call)$matrix))

    ## One tree multiple data works
    test <- check.tree(tree = tree, data = data, bind.trees = FALSE, match_call)$tree
    expect_is(test, "multiPhylo")
    expect_is(test[[1]], "phylo")
    expect_equal(length(test), 1)

    ## multiple tree multiple data
    tree <- makeNodeLabel(rtree(5))
    tree <- list(tree, tree)
    class(tree) <- "multiPhylo"
    data <- matrix(0, nrow = 9, ncol = 2, dimnames = list(c(paste0("t", 1:5), paste0("Node", 1:4))))
    data <- fill.dispRity(make.dispRity(data = check.data(list(data, data), match_call)$matrix))

    ## multiple tree multiple data works
    test <- check.tree(tree = tree, data = data, bind.trees = FALSE, match_call)$tree
    expect_is(test, "multiPhylo")
    expect_is(test[[1]], "phylo")
    expect_equal(length(test), 2)

    ## Binding works
    error <- capture_error(check.tree(tree = tree[[1]], data = data, bind.trees = TRUE, match_call)$tree)
    expect_equal(error[[1]], "The number of matrices and trees must be the same to bind them.")
    wrong_tree <- tree[[1]]
    wrong_tree$tip.label[1:2] <- letters[1:2]

    ## Is set to multi
    warn <- capture_warning(check.tree(tree = wrong_tree, data = data))
    expect_equal(warn[[1]], "The following elements are not present in all trees: t1, t4, a, b. Some analyses downstream might not work because of this (you can use ?clean.data to match both data and tree if needed).")
    expect_warning(test <- check.tree(tree = wrong_tree, data = data))
    expect_false(test$multi)

    ## Different trees work
    set.seed(1)
    tree <- makeNodeLabel(rtree(5))
    tree <- list(tree, tree)
    class(tree) <- "multiPhylo"
    data <- matrix(0, nrow = 9, ncol = 2, dimnames = list(c(paste0("t", 1:5), paste0("Node", 1:4))))
    data2 <- fill.dispRity(make.dispRity(data = check.data(list(data, data), match_call)$matrix))
    data1 <- fill.dispRity(make.dispRity(data = check.data(data, match_call)$matrix))

    tree_trifurc <- tree[[1]]
    tree_trifurc$edge <- tree_trifurc$edge[-5, ]
    tree_trifurc$edge[c(5,6),1] <- 8
    tree_trifurc$edge.length <- tree_trifurc$edge.length[-5] 
    tree_trifurc$Nnode <- 3
    tree_trifurc$node.label <- tree_trifurc$node.label[-4]
    tree <- list(tree[[1]], tree_trifurc)
    class(tree) <- "multiPhylo"
     
    ## Outputs the multi part   
    test <- check.tree(tree[[1]], data1, bind.trees = FALSE, match_call)
    expect_is(test, "list")
    expect_equal(names(test), c("tree", "multi"))
    expect_false(test$multi)

    ## Nodes are different between the trees but all match the one matrix
    warn <- capture_warning(check.tree(tree, data1, bind.trees = FALSE, match_call))
    expect_equal(warn[[1]], "The following elements are not present in all trees: Node4. Some analyses downstream might not work because of this (you can use ?clean.data to match both data and tree if needed).")
    expect_warning(test <- check.tree(tree, data1, bind.trees = FALSE, match_call))
    expect_false(test$multi)

    ## Nodes are different between the trees and match the individual matrices
    error <- capture_error(check.tree(tree, data2, bind.trees = FALSE, match_call))
    expect_equal(error[[1]], "The data is not matching the tree labels (you can use ?clean.data to match both data and tree).")
    error <- capture_error(check.tree(tree, data2, bind.trees = TRUE, match_call))
    expect_equal(error[[1]], "The data is not matching the tree labels (you can use ?clean.data to match both data and tree).")
})

## Test the check.dispRity.data generic
test_that("check.dispRity.data works", {
    match_call <- list(tree = "my_tree", data = "my_data")
    ## One tree one data
    tree <- makeNodeLabel(rtree(5))
    data <- matrix(0, nrow = 9, ncol = 2, dimnames = list(c(paste0("t", 1:5), paste0("Node", 1:4))))
    data_formed <- fill.dispRity(make.dispRity(data = check.data(data, match_call)$matrix))

    ## Just data
    test <- check.dispRity.data(data = data, returns = "matrix")
    expect_is(test, "list")
    expect_is(test[[1]], "matrix")

    ## Just tree
    test <- check.dispRity.data(data = data, tree = tree, returns = "tree")
    expect_is(test, "multiPhylo")
    expect_is(test[[1]], "phylo")

    ## Correct returns
    test <- check.dispRity.data(data = data, tree = tree, returns = c("tree", "multi", "matrix"))
    expect_equal(names(test), c("matrix", "tree", "multi"))
    test <- check.dispRity.data(data = data, tree = tree, returns = c("tree", "matrix"))
    expect_equal(names(test), c("matrix", "tree"))
    test <- check.dispRity.data(data = data, tree = tree, returns = c("multi", "matrix"))
    expect_equal(names(test), c("matrix", "multi"))
})

