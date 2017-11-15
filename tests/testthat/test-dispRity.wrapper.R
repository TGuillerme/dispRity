#TEST disparity through time wrapper
context("dispRity.wrapper")

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
        , c("133.51104 - 89.00736", "89.00736 - 44.50368", "44.50368 - 0"))
    
    ## Correct values
    expect_equal(
        summary(result)$obs
        , c(1.084, 1.348, 1.353))
    expect_equal(
        summary(result)$bs.median
        , c(1.026, 1.328, 1.321))

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
        , c(1.387, 1.274))
    expect_equal(
        summary(result)$bs.median
        , c(1.368, 1.252))
})