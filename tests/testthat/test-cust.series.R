## TESTING cust.series

context("cust.series")

data <- matrix(data = NA, nrow = 10, ncol = 9)
rownames(data) <- letters[1:10]

## Sanitizing
test_that("Sanitizing works", {
    ## class
    expect_error(
        cust.series(data, factor = "A")
        )
    ## same number of rows
    factor <- matrix(5,5)
    expect_error(
        cust.series(data, factor)
        )
    factor <- as.data.frame(matrix(data = c(rep(1,5), rep(2,5)), nrow = 10, ncol = 1))
    expect_error(
        cust.series(data, factor)
        )
    ## row names must be the same
    rownames(factor) <- letters[2:11]
    expect_error(
        cust.series(data, factor)
        )
    ## One class with only 3 elements
    rownames(factor) <- letters[1:10]
    factor[1:2,1] <- 3
    expect_error(
        cust.series(data, factor)
        )
})

## Results
factor <- as.data.frame(matrix(data = c(rep(1,5), rep(2,5)), nrow = 10, ncol = 1))
rownames(factor) <- letters[1:10]
test <- cust.series(data, factor)

## Test
test_that("cust.series works", {
    expect_is(
        test
        , "dispRity")
    expect_equal(
        length(test)
        , 3)
    expect_is(
        test$matrix
        , "matrix")
    expect_equal(
        dim(test$matrix)
        , c(10,9))
    expect_equal(
        rownames(test$matrix[test$series$origin$elements,])
        , rownames(data))
    expect_equal(
        length(test$series[[2]]$elements)
        ,5)
    expect_equal(
        length(test$series[[3]]$elements)
        ,5)
})

## Example
test_that("Example works", {
    ordinated_matrix  <-  matrix(data  =  rnorm(90), nrow  =  10, ncol  =  9, dimnames  =  list(letters[1:10]))
    factors  <-  as.data.frame(matrix(data  =  c(rep(1,5), rep(2,5)), nrow  =  10, ncol  =  1, dimnames  =  list(letters[1:10])))
    ex1 <- cust.series(ordinated_matrix, factors)
    expect_is(
        ex1
        , "dispRity")
    expect_equal(
        length(ex1)
        ,3)
    expect_equal(
        dim(ex1$matrix[ex1$series[[2]]$elements,])
        , c(5,9))
})