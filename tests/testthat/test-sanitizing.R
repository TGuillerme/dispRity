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

## Test check.dispRity.data
test_that("check.dispRity.data works", {


    ## All errors
    error <- capture_error(check.dispRity.data("a"))
    expect_equal(error[[1]], "data must be of class matrix or data.frame or list.")
    error <- capture_error(check.dispRity.data(list(matrix(c(1,2)), "a")))
    expect_equal(error[[1]], "list(matrix(c(1, 2)), \"a\") must be matrix or a list of matrices with the same dimensions and row names.")
    error2 <- list(matrix(c(1,2)), matrix(c(1,2,3)))
    error <- capture_error(check.dispRity.data(error2))
    expect_equal(error[[1]], "error2 must be matrix or a list of matrices with the same dimensions and row names.")
    error3 <- list(matrix(c(1,2), dimnames = list(c(1:2), 1)), matrix(c(1,2), dimnames = list(c(3:4), 1)))
    error <- capture_error(check.dispRity.data(error3))
    expect_equal(error[[1]], "error3 must be matrix or a list of matrices with the same dimensions and row names.")

    ## Matrix input
    bob <- matrix(c(1,2))
    warn <- capture_warnings(test <- check.dispRity.data(bob))
    expect_equal(warn[[1]], "Row names have been automatically added to bob.")
    expect_is(test, "list")
    expect_is(test[[1]], "matrix")
    expect_equal(dim(test[[1]]), c(2,1))
    expect_equal(rownames(test[[1]]), c("1","2"))

    bob <- matrix(c(1,2))
    rownames(bob) <- c(1,2)
    test <- check.dispRity.data(bob)
    expect_is(test, "list")
    expect_is(test[[1]], "matrix")
    expect_equal(dim(test[[1]]), c(2,1))
    expect_equal(rownames(test[[1]]), c("1","2"))

    ## List input
    bib <- list(matrix(c(1,2)))
    warn <- capture_warnings(test <- check.dispRity.data(bib))
    expect_equal(warn[[1]], "Row names have been automatically added to bib.")
    expect_is(test, "list")
    expect_is(test[[1]], "matrix")
    expect_equal(dim(test[[1]]), c(2,1))
    expect_equal(rownames(test[[1]]), c("1","2"))

    bob <- list(matrix(c(1,2)), matrix(c(1,2)))
    warn <- capture_warnings(test <- check.dispRity.data(bob))
    expect_equal(warn[[1]], "Row names have been automatically added to bob.")
    expect_is(test, "list")
    expect_is(test[[1]], "matrix")
    expect_equal(dim(test[[1]]), c(2,1))
    expect_equal(dim(test[[2]]), c(2,1))
    expect_equal(rownames(test[[1]]), c("1","2"))
    expect_equal(rownames(test[[2]]), c("1","2"))

    bub <- list(matrix(c(1,2), dimnames = list(c(1:2), 1)), matrix(c(1,2), dimnames = list(c(2:1), 1)))
    test <- check.dispRity.data(bub)
    expect_is(test, "list")
    expect_is(test[[1]], "matrix")
    expect_equal(dim(test[[1]]), c(2,1))
    expect_equal(dim(test[[2]]), c(2,1))
    expect_equal(rownames(test[[1]]), c("1","2"))
    expect_equal(rownames(test[[2]]), c("1","2"))
})

## Test check.dispRity.phy
test_that("check.dispRity.phy works", {    
    ## One tree one data
    phy <- makeNodeLabel(rtree(5))
    data <- matrix(0, nrow = 9, ncol = 2, dimnames = list(c(paste0("t", 1:5), paste0("Node", 1:4))))
    data <- fill.dispRity(make.dispRity(data = check.dispRity.data(data)))

    ## Basic error
    error <- capture_error(check.dispRity.phy(phy = "phy", data = data))
    expect_equal(error[[1]], "phy must be of class phylo or multiPhylo.")
    ## Basic works
    test <- check.dispRity.phy(phy = phy, data = data)
    expect_is(test, "multiPhylo")
    expect_is(test[[1]], "phylo")
    expect_equal(length(test), 1)

    ## Multiple tree one data
    phy <- makeNodeLabel(rtree(5))
    phy <- list(phy, phy)
    class(phy) <- "multiPhylo"
    data <- matrix(0, nrow = 9, ncol = 2, dimnames = list(c(paste0("t", 1:5), paste0("Node", 1:4))))
    # data <- fill.dispRity(make.dispRity(data = check.dispRity.data(data)))

    ## Not all trees have node labels
    phy_error <- phy
    phy_error[[1]]$node.label <- NULL
    error <- capture_error(check.dispRity.phy(phy = phy_error, data = data))
    expect_equal(error[[1]], "All trees should have node labels or no node labels.")
    ## multiple trees works
    test <- check.dispRity.phy(phy = phy, data = data)
    expect_is(test, "multiPhylo")
    expect_is(test[[1]], "phylo")
    expect_equal(length(test), 2)

    ## One tree multiple data
    phy <- makeNodeLabel(rtree(5))
    data <- matrix(0, nrow = 9, ncol = 2, dimnames = list(c(paste0("t", 1:5), paste0("Node", 1:4))))
    data <- fill.dispRity(make.dispRity(data = check.dispRity.data(list(data, data))))

    ## One tree multiple data works
    test <- check.dispRity.phy(phy = phy, data = data)
    expect_is(test, "multiPhylo")
    expect_is(test[[1]], "phylo")
    expect_equal(length(test), 1)

    ## multiple tree multiple data
    phy <- makeNodeLabel(rtree(5))
    phy <- list(phy, phy)
    class(phy) <- "multiPhylo"
    data <- matrix(0, nrow = 9, ncol = 2, dimnames = list(c(paste0("t", 1:5), paste0("Node", 1:4))))
    data <- fill.dispRity(make.dispRity(data = check.dispRity.data(list(data, data))))

    ## multiple tree multiple data works
    test <- check.dispRity.phy(phy = phy, data = data)
    expect_is(test, "multiPhylo")
    expect_is(test[[1]], "phylo")
    expect_equal(length(test), 2)
})