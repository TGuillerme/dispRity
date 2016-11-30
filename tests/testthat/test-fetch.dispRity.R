context("fetch.dispRity")

set.seed(1)
ordinated_matrix <- matrix(data = rnorm(90), nrow = 10, ncol = 9, dimnames = list(letters[1:10]))
factors <- as.data.frame(matrix(data = c(rep(1,5), rep(2,5)), nrow = 10, ncol = 1, dimnames = list(letters[1:10])))
matrix_list <- cust.series(ordinated_matrix, factors)
test1 <- boot.matrix(ordinated_matrix, bootstraps = 2)
test2 <- boot.matrix(matrix_list, bootstraps = 2, rarefaction = c(6,7))

test_that("fetch.matrix works", {
    expect_equal(
        fetch.matrix(test1)
        ,ordinated_matrix)
    expect_error(
        fetch.matrix(test1, 1)
        )
    expect_equal(
        rownames(fetch.matrix(test1, 0, 0, 2))
        ,c("h","d","b","j","c","f","b","i","d","h"))
    expect_equal(
        fetch.matrix(test2)
        ,ordinated_matrix)
    expect_equal(
        rownames(fetch.matrix(test2, 1))
        ,letters[1:5])
    expect_equal(
        rownames(fetch.matrix(test2, 2))
        ,letters[6:10])
    expect_equal(
        rownames(fetch.matrix(test2, 1, 1, 1))
        ,c("b","c","b","e","c","b"))
})

test_that("fetch.elements works", {
    expect_equal(
        fetch.elements(test1)
        , seq(1:10))
    expect_error(
        fetch.elements(test1, 1)
        )
    expect_equal(
        fetch.elements(test2)
        , seq(1:10))
    expect_equal(
        fetch.elements(test2, 1)
        , seq(1:5))
})

test_that("fetch.series works", {
    expect_equal(
        length(fetch.series(test1))
        ,0)
    expect_equal(
        fetch.series(test2)
        ,c("V1.1", "V1.2"))
})