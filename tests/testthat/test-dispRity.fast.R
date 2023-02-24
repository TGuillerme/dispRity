## Test

test_that("dispRity.fast works", {
    ## Basic testing
    space <- matrix(1, 5, 5)
    metric1 <- sum
    metric2 <- c(sum, variances)
    metric3 <- variances
    metric4 <- centroids
    group <- c(TRUE, TRUE, TRUE, TRUE, FALSE)
    ## Testing metrics
    expect_equal(dispRity.fast(group, space, metric1), 20)
    expect_equal(dispRity.fast(group, space, metric2), 0)
    expect_equal(dispRity.fast(group, space, metric3), c(0,0,0,0,0))
    expect_equal(dispRity.fast(group, space, metric4), c(0,0,0,0))

    ## Handling arguments
    expect_equal_round(dispRity.fast(group, space, metric4, centroid = 100), c(221.3707, 221.3707, 221.3707, 221.3707), 4)
    expect_equal_round(dispRity.fast(group, space, c(mean, metric4), centroid = 100), 221.3707, 4)

    ## Include args
    
})
