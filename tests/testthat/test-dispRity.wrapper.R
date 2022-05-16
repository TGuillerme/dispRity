#TEST disparity through time wrapper
#context("dispRity.wrapper")

#Test
test_that("Wrapping works", {
    set.seed(1)
    data(BeckLee_mat50) ; data(BeckLee_tree)
    result <- dispRity.through.time(BeckLee_mat50, BeckLee_tree, 3)

    ## Correct class
    expect_is(
        result, 'dispRity')

    ## Correct subsets    
    expect_equal(
        names(result$subsets)
        , c("133.51 - 89.01", "89.01 - 44.5", "44.5 - 0"))
    
    ## Correct values
    expect_equal(
        summary(result)$obs
        , c(1.269, 1.521, 1.533))
    expect_equal(
        summary(result)$bs.median
        , c(1.157, 1.510, 1.501))

    set.seed(1)
    data(BeckLee_mat50) ; data(BeckLee_tree)
    result <- dispRity.per.group(BeckLee_mat50, list(crown = c(16, 19:41, 45:50), stem = c(1:15, 17:18, 42:44)))

    ## Correct class
    expect_is(
        result, 'dispRity')

    ## Correct subsets    
    expect_equal(
        names(result$subsets)
        , c("crown", "stem"))
    
    ## Correct values
    expect_equal(
        summary(result)$obs
        , c(1.554, 1.457))
    expect_equal(
        summary(result)$bs.median
        , c(1.541, 1.427))
})