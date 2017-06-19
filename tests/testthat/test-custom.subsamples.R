## TESTING custom.subsamples

context("custom.subsamples")

data <- matrix(data = rnorm(10*9), nrow = 10, ncol = 9)
rownames(data) <- letters[1:10]

## Sanitizing
test_that("Sanitizing works", {
    ## class
    expect_error(
        custom.subsamples(data, group = "A")
        )
    ## same number of rows
    group <- matrix(5,5)
    expect_error(
        custom.subsamples(data, group)
        )
    group <- as.data.frame(matrix(data = c(rep(1,5), rep(2,5)), nrow = 10, ncol = 1))
    expect_error(
        custom.subsamples(data, group)
        )
    ## row names must be the same
    rownames(group) <- letters[2:11]
    expect_error(
        custom.subsamples(data, group)
        )
    ## One class with only 3 elements
    rownames(group) <- letters[1:10]
    group[1:2,1] <- 3
    expect_error(
        custom.subsamples(data, group)
        )
})

## Results
group <- as.data.frame(matrix(data = c(rep(1,5), rep(2,5)), nrow = 10, ncol = 1))
rownames(group) <- letters[1:10]
test <- custom.subsamples(data, group)

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

## Test
test_that("Different group inputs gives the same output", {

    group1 <- list("A" = c(1,2,3,4), "B" = c(5,6,7,8,9,10))
    group2 <- list("A" = c("a", "b", "c", "d"), "B" = c(letters[5:10]))
    group3 <- as.data.frame(matrix(data = c(rep(1,4), rep(2,6)), nrow = 10, ncol = 1, dimnames = list(letters[1:10])))

    cust1 <- custom.subsamples(data, group1)
    cust2 <- custom.subsamples(data, group2)
    cust3 <- custom.subsamples(data, group3)

    expect_equal(
        unique(unlist(lapply(list(cust1, cust2, cust3), class)))
        , "dispRity")
    expect_equal(
        unique(unlist(lapply(list(cust1, cust2, cust3), length)))
        , 3)

    expect_true(
        all(as.vector(cust1$subsamples[[1]]$elements) == as.vector(cust2$subsamples[[1]]$elements))
    )
    expect_true(
        all(as.vector(cust1$subsamples[[1]]$elements) == as.vector(cust3$subsamples[[1]]$elements))
    )
    expect_true(
        all(as.vector(cust2$subsamples[[1]]$elements) == as.vector(cust3$subsamples[[1]]$elements))
    )
    expect_true(
        all(as.vector(cust1$subsamples[[2]]$elements) == as.vector(cust2$subsamples[[2]]$elements))
    )
    expect_true(
        all(as.vector(cust1$subsamples[[2]]$elements) == as.vector(cust3$subsamples[[2]]$elements))
    )
    expect_true(
        all(as.vector(cust2$subsamples[[2]]$elements) == as.vector(cust3$subsamples[[2]]$elements))
    )
})


## Example
test_that("Example works", {
    ordinated_matrix <- matrix(data = rnorm(90), nrow = 10, ncol = 9, dimnames = list(letters[1:10]))
    ## Splitting the ordinated matrix into two groups using row numbers
    numbers <- custom.subsamples(ordinated_matrix, list("A" = c(1:4), "B" = c(5:10)))
    ## Splitting the ordinated matrix into three groups using row names
    Letters <- custom.subsamples(ordinated_matrix, list("A" = c("a", "b", "c", "d"), "B" = c("e", "f", "g", "h", "i", "j"), "C" = c("a", "c", "d", "f", "h")))
    ## Splitting the ordinated matrix into four groups using a data frame
    groups <- as.data.frame(matrix(data = c(rep(1,5), rep(2,5), rep(c(1,2), 5)), nrow = 10, ncol = 2, dimnames = list(letters[1:10], c("g1", "g2"))))
    dataframe <- custom.subsamples(ordinated_matrix, groups)

    expect_is(numbers, "dispRity")
    expect_is(Letters, "dispRity")
    expect_is(dataframe, "dispRity")

    expect_equal(
        as.vector(unlist(lapply(numbers$subsamples, lapply, length)))
        , c(4,6))
    expect_equal(
        names(unlist(lapply(numbers$subsamples, lapply, length)))
        , c("A.elements", "B.elements"))

    expect_equal(
        as.vector(unlist(lapply(Letters$subsamples, lapply, length)))
        , c(4,6,5))
    expect_equal(
        names(unlist(lapply(Letters$subsamples, lapply, length)))
        , c("A.elements", "B.elements", "C.elements"))
    expect_equal(
        as.vector(unlist(lapply(dataframe$subsamples, lapply, length)))
        , c(rep(5,4)))
    expect_equal(
        names(unlist(lapply(dataframe$subsamples, lapply, length)))
        , c("g1.1.elements", "g1.2.elements", "g2.1.elements", "g2.2.elements"))
})