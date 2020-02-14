## TEST sanitizing

context("sanitizing")

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

## Test check.matrix
test_that("test.matrix works", {

    data1 <- matrix(c(rnorm(4), runif(4)), 4, 2, dimnames = list(LETTERS[1:4], c("var1", "var2")))
    data2 <- matrix(c(rnorm(5), runif(5)), 5, 2, dimnames = list(LETTERS[1:5], c("var1", "var2")))
    data3 <- matrix(c(rnorm(4), runif(4)), 4, 2, dimnames = list(LETTERS[2:5], c("var1", "var2")))
    data_long <- list(data1, data1)
    tree1 <- rtree(4, tip.label = LETTERS[1:4])
    tree_long <- list(rtree(5, tip.label = LETTERS[1:5]), rtree(4, tip.label = LETTERS[1:4]), rtree(6, tip.label = LETTERS[1:6]))
    class(tree_long) <- "multiPhylo"

    ## Test pass
    expect_equal(check.matrix(list(data1)), 1)
    expect_equal(check.matrix(data_long), 2)
    expect_equal(check.matrix(list(data1), tree = tree1), 1)
    expect_equal(check.matrix(list(data1, data1), tree = tree1), 2)

    ## Test fails
    error <- capture_error(check.matrix(list("A", "B")))
    expect_equal(error[[1]], "list(\"A\", \"B\") must be a matrix or a list of matrices.")
    error <- capture_error(check.matrix(data_long, tree = tree_long))
    expect_equal(error[[1]], "data_long does not match tree_long. Use the following to match both:\n    clean.data(data_long, tree_long)")
    error <- capture_error(check.matrix(list(data1, data2)))
    expect_equal(error[[1]], "Some matrices in list(data1, data2) have different dimensions.")
    error <- capture_error(check.matrix(list(data1, data3)))
    expect_equal(error[[1]], "All matrices in list(data1, data3) must have the same rownames.")

    ## Counting works
    expect_equal(check.matrix(list(data1), count = TRUE), list(c(4,2)))
    expect_equal(check.matrix(list(data2), count = TRUE), list(c(5,2)))
    expect_equal(check.matrix(data_long, count = TRUE), list(c(4,2), c(5,2)))
})