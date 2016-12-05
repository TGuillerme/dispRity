## TESTING boot.matrix

context("boot.dispRity")

## Loading the data
load("test_data.Rda")
data <- test_data$ord_data_tips


## Testing boot.matrix with a single matrix input
bootstraps = 5
rarefaction = FALSE
boot.type = "full"
## Sanitizing
test_that("Sanitizing works correctly", {
    expect_error(
        boot.matrix(data = "a", bootstraps, rarefaction, dimensions = FALSE, verbose = FALSE, boot.type = "full")
        )
    expect_error(
        boot.matrix(data, bootstraps = FALSE, rarefaction, dimensions = FALSE, verbose = FALSE, boot.type = "full")
        )
    expect_error(
        boot.matrix(data, bootstraps = "a", rarefaction, dimensions = FALSE, verbose = FALSE, boot.type = "full")
        )
    expect_error(
        boot.matrix(data, bootstraps, rarefaction = "a", dimensions = FALSE, verbose = FALSE, boot.type = "full")
        )
    expect_error(
        boot.matrix(data, bootstraps, rarefaction, dimensions = -1, verbose = FALSE, boot.type = "full")
        )
    expect_error(
        boot.matrix(data, bootstraps, rarefaction, dimensions = FALSE, verbose = 8, boot.type = "full")
        )
    expect_error(
        boot.matrix(data, bootstraps, rarefaction, dimensions = FALSE, verbose = FALSE, boot.type = "rangers")
        )
    expect_error(
        boot.matrix(data, bootstraps, rarefaction, dimensions = FALSE, verbose = FALSE, boot.type = 2)
        )
    expect_error(
        boot.matrix(data, bootstraps, rarefaction, dimensions = FALSE, verbose = FALSE, boot.type = "full", parallel = TRUE)
        )
})

## No bootstrap (is equal to the matrix)
test_that("No bootstraps", {
    test <- boot.matrix(data, bootstraps = 0)
    expect_is(
        test
        , "dispRity")
    expect_equal(
        length(test)
        , 3)
    expect_equal(
        test$series$origin$elements
        , seq(1:nrow(test$matrix)))
    expect_equal(
        test$call$dimensions
        , ncol(test$matrix))
    expect_equal(
        length(test$series[[1]])
        ,1)
    expect_equal(
        length(test$series)
        ,2)
    expect_equal(
        length(test$series[[2]])
        ,2)
    expect_is(
        test$series[[2]][[2]]
        ,"matrix")
})

## No bootstrap but remove dimensions
test_that("Remove dimensions", {
    expect_equal(
        boot.matrix(data, bootstraps = 0, dimensions = 0.5)$call$dimensions
        ,24)
    expect_equal(
        boot.matrix(data, bootstraps = 0, dimensions = 24)$call$dimensions
        ,24)
})


## Bootstraps = 5
test_that("5 bootstraps", {
    data <- test_data$ord_data_tips
    test <- boot.matrix(data, bootstraps = 5)
    expect_is(
        test
        , "dispRity")
    expect_equal(
        length(test)
        , 3)
    expect_equal(
        test$series$origin$elements
        , seq(1:nrow(test$matrix)))
    expect_equal(
        test$call$dimensions
        , ncol(test$matrix))
    expect_equal(
        dim(test$series[[2]][[2]])
        ,c(50,5))
    expect_equal(
        length(test$series[[2]])
        ,2)
})

## Bootstraps = 5 + Rarefaction = 5
test_that("5 bootstraps, rarefaction = 5", {
    test <- boot.matrix(data, bootstraps = 5, rarefaction = 5)
    expect_is(
        test
        , "dispRity")
    expect_equal(
        length(test)
        , 3)
    expect_equal(
        test$series$origin$elements
        , seq(1:nrow(test$matrix)))
    expect_equal(
        test$call$dimensions
        , ncol(test$matrix))
    expect_equal(
        dim(test$series[[2]][[2]])
        ,c(50,5))
    expect_equal(
        dim(test$series[[2]][[3]])
        ,c(5,5))
})

## Bootstraps = 5 + Rarefaction = TRUE
test_that("5 bootstraps, rarefaction = TRUE", {
    test <- boot.matrix(data, bootstraps = 5, rarefaction = TRUE)
    expect_equal(
        length(test$series[[2]])
        , 50)
    for(rare in 3:50) {
        expect_equal(
            dim(test$series[[2]][[rare]])
            ,c(50-(rare-3),5))
    }
})

## Bootstraps = 5 + Rarefaction = c(5,6) + boot.type
test_that("5 bootstraps, rarefaction = 5,6, boot type", {
    test <- boot.matrix(data, bootstraps = 5, rarefaction = c(5,6), boot.type = "single")
    expect_equal(
        test$call$bootstrap[[1]]
        , 5)
    expect_equal(
        test$call$bootstrap[[2]]
        , "single")
    expect_equal(
        test$call$bootstrap[[3]]
        , c(5,6))
})


## Bootstraps = 5 + Rarefaction = c(5,6) + series
test_that("5 bootstraps, rarefaction = 5,6, series", {
    ordinated_matrix <- matrix(data = rnorm(90), nrow = 10, ncol = 9, dimnames = list(letters[1:10]))
    factors <- as.data.frame(matrix(data = c(rep(1,5), rep(2,5)), nrow = 10, ncol = 1, dimnames = list(letters[1:10])))
    matrix_list <- cust.series(ordinated_matrix, factors)
    test <- boot.matrix(matrix_list, bootstraps = 2, rarefaction = c(6,7))
    expect_is(
        test
        , "dispRity")
    expect_equal(
        length(test)
        , 3)
    expect_equal(
        test$series$origin$elements
        , seq(1:nrow(test$matrix)))
    expect_equal(
        test$call$dimensions
        , ncol(test$matrix))
    expect_equal(
        length(test$series)
        ,3)
    expect_equal(
        length(test$series[[1]])
        ,1)
    expect_equal(
        length(test$series[[2]])
        ,4)
    expect_equal(
        length(test$series[[3]])
        ,4)
    expect_equal(
        dim(test$series[[3]][[2]])
        ,c(length(test$series[[3]]$elements), 2))
})

