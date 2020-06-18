context("reduce.space")

# stop("DEBUG")
# library(testthat)
# library(dispRity)
# source("../reduce.space.R")
# source("../reduce.space_fun.R")



## Test
test_that("reduce.space works", {

    ## Sanitizing
    expect_error(reduce.space())

    set.seed(42)
    space <- dispRity::space.maker(300, 2, distribution = rnorm)

    ## Random removal, super easy
    test <- reduce.space(space, type = "random", remove = 0.3)
    expect_is(test, "logical")
    expect_equal(length(test), 300)
    expect_equal(length(which(test)), 90)

    ## Limit removal
    set.seed(1)
    iter <- capture_output(test1 <- reduce.space(space, type = "limit", remove = 0.5, verbose = TRUE))
    expect_is(test1, "logical")
    expect_equal(length(test1), 300)
    expect_equal(length(which(test1)), 151)
    expect_equal(iter, "Run parameter optimisation:........Done.")

    set.seed(1)
    test2 <- reduce.space(space, type = "limit", parameters = list("radius" = 1.21875))
    expect_is(test2, "logical")
    expect_equal(length(test2), 300)
    expect_equal(length(which(test2)), 151)

    expect_equal(test1, test2)

    set.seed(1)
    test3 <- reduce.space(space, type = "limit", remove = 0.5, return.optim = TRUE)
    expect_equal(test3[[2]], 1.21875)
    expect_equal(length(which(test3[[1]])), 151)

    expect_equal(test1, test3$remove)




    ## Displacement removal
    set.seed(1)
    iter <- capture_output(test1 <- reduce.space(space, type = "displacement", remove = 0.5, verbose = TRUE, tuning = list("tol" = 0.1)))
    expect_is(test1, "logical")
    expect_equal(length(test1), 300)
    expect_equal(length(which(test1)), 152)
    expect_equal(iter, "Run parameter optimisation:........Done.")

    set.seed(1)
    test2 <- reduce.space(space, type = "displacement", parameters = list("radius" = 4.390509))
    expect_is(test2, "logical")
    expect_equal(length(test2), 300)
    expect_equal(length(which(test2)), 152)

    expect_equal(test1, test2)

    set.seed(1)
    test3 <- reduce.space(space, type = "displacement", remove = 0.5, return.optim = TRUE, tuning = list("tol" = 0))
    expect_equal(round(test3[[2]], 6), 4.390509)
    expect_equal(length(which(test3[[1]])), 152)


    ## Density removal
    set.seed(1)
    iter <- capture_output(test1 <- reduce.space(space, type = "density", remove = 0.5, verbose = TRUE))
    expect_is(test1, "logical")
    expect_equal(length(test1), 300)
    expect_equal(length(which(test1)), 151)
    expect_equal(iter, "Run parameter optimisation:............Done.")

    set.seed(1)
    test2 <- reduce.space(space, type = "density", parameters = list("diameter" = 0.1015625))
    expect_is(test2, "logical")
    expect_equal(length(test2), 300)
    expect_equal(length(which(test2)), 151)

    expect_equal(test1, test2)

    set.seed(1)
    test3 <- reduce.space(space, type = "density", remove = 0.5, return.optim = TRUE)
    expect_equal(test3[[2]], 0.1015625)
    expect_equal(length(which(test3[[1]])), 151)

    expect_equal(test1, test3$remove)

})
