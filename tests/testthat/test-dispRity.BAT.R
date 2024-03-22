
test_that("make.BAT.comm works", {
    dummy_matrix <- matrix(rnorm(90), 10, 9, dimnames = list(letters[1:10]))
    sub_matrix <- dummy_matrix[c(1:3), ]
    test <- make.BAT.comm(sub_matrix)
    expect_equal(dim(test), c(1,3))
    expect_equal(sum(test), 3)
    test <- make.BAT.comm(sub_matrix, data = dummy_matrix)
    expect_equal(dim(test), c(1,10))
    expect_equal(sum(test), 3)
})
