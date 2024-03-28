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

test_that("dispRity.BAT works", {
    ## Test convert simple
    dummy_matrix <- matrix(rnorm(90), 10, 9, dimnames = list(letters[1:10]))
    data <- make.dispRity(dummy_matrix)
    out <- dispRity.BAT(data)
    expect_is(out, "list")
    expect_equal(names(out), c("comm", "tree", "traits"))
    expect_equal(dim(out$traits), c(10,9))
    expect_equal(dim(out$comm), c(1,10))
    expect_true(is.null(out$tree))

    ## Test convert complex
    data(demo_data)
    eco_data <- demo_data$jones    
    out <- dispRity.BAT(eco_data)
    expect_is(out, "list")
    expect_equal(names(out), c("comm", "tree", "traits"))
    expect_equal(dim(out$traits), c(48,47))
    expect_equal(dim(out$comm), c(2,48))
    expect_true(is.null(out$tree))

    ## Test convert complex
    data(disparity)
    out <- dispRity.BAT(disparity)
    expect_is(out, "list")
    expect_equal(names(out), c("comm", "tree", "traits"))
    expect_equal(dim(out$traits), c(99,97))
    expect_equal(dim(out$comm), c(7,99))
    expect_is(out$tree, "phylo")
})
