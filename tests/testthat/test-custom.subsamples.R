## TESTING custom.subsamples

context("custom.subsamples")

data <- matrix(data = rnorm(10*9), nrow = 10, ncol = 9)
rownames(data) <- letters[1:10]

## Sanitizing
test_that("Sanitizing works", {
    ## class
    expect_error(
        custom.subsamples(data, factor = "A")
        )
    ## same number of rows
    factor <- matrix(5,5)
    expect_error(
        custom.subsamples(data, factor)
        )
    factor <- as.data.frame(matrix(data = c(rep(1,5), rep(2,5)), nrow = 10, ncol = 1))
    expect_error(
        custom.subsamples(data, factor)
        )
    ## row names must be the same
    rownames(factor) <- letters[2:11]
    expect_error(
        custom.subsamples(data, factor)
        )
    ## One class with only 3 elements
    rownames(factor) <- letters[1:10]
    factor[1:2,1] <- 3
    expect_error(
        custom.subsamples(data, factor)
        )
})

## Results
factor <- as.data.frame(matrix(data = c(rep(1,5), rep(2,5)), nrow = 10, ncol = 1))
rownames(factor) <- letters[1:10]
test <- custom.subsamples(data, factor)

## Test
test_that("custom.subsamples works", {
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
        length(test$subsamples[[1]]$elements)
        ,5)
    expect_equal(
        length(test$subsamples[[2]]$elements)
        ,5)
})

## Example
test_that("Example works", {
    ordinated_matrix  <-  matrix(data  =  rnorm(90), nrow  =  10, ncol  =  9, dimnames  =  list(letters[1:10]))
    factors  <-  as.data.frame(matrix(data  =  c(rep(1,5), rep(2,5)), nrow  =  10, ncol  =  1, dimnames  =  list(letters[1:10])))
    ex1 <- custom.subsamples(ordinated_matrix, factors)
    expect_is(
        ex1
        , "dispRity")
    expect_equal(
        length(ex1)
        ,3)
    expect_equal(
        dim(ex1$matrix[ex1$subsamples[[2]]$elements,])
        , c(5,9))
})