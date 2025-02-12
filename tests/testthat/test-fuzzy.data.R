

test_that("matrix.style works", {
    expect_equal(matrix.style(1, 1, 5), 1) # 1*1
    expect_equal(matrix.style(1, 2, 5), 6) # 1 + dims[1]*(2-1)
    expect_equal(matrix.style(2, 2, 5), 7) # 2 + dims[1]*(2-1)
    expect_equal(matrix.style(4, 3, 5), 14)
    expect_equal(matrix.style(5, 4, 5), 20)
})

test_that("simple matrix works", {
    ## Transform a boring matrix into a fuzz one

    data1 <- matrix(1:20, 5, 4)
    data2 <- data1+0.1
    data_list <- list(data1, data2)
    test <- fuzz.data(data)
    expect_is(test, "fuzzy.matrix")

})


test_that("fuzzy.data works", {
})
