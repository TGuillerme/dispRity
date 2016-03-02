context("scale.dispRity")


# Example data
data(BeckLee_mat50)
factors <- as.data.frame(matrix(data = c(rep(1, nrow(BeckLee_mat50)/2), rep(2, nrow(BeckLee_mat50)/2)), nrow = nrow(BeckLee_mat50), ncol = 1, dimnames = list(rownames(BeckLee_mat50))))
customised_series <- cust.series(BeckLee_mat50, factors)
bootstrapped_data <- boot.matrix(customised_series, bootstraps = 7, rarefaction = c(10, 25))
disparity1 <- dispRity(BeckLee_mat50, metric = c(sum, centroids))
disparity2 <- dispRity(customised_series, metric = c(sum, centroids))
disparity3 <- dispRity(bootstrapped_data, metric = c(sum, centroids))


#Test
test_that("scale.dispRity works", {
    #class - single
    expect_error(
        scale.dispRity("bla")
        )
    expect_error(
        scale.dispRity(matrix(rnorm(10)))
        )
    #returns the right objects
    expect_is(
        scale.dispRity(disparity1)
        , "dispRity")
    expect_is(
        scale.dispRity(disparity2)
        , "dispRity")
    expect_is(
        scale.dispRity(disparity3)
        , "dispRity")
    expect_equal(
        length(scale.dispRity(disparity1))
        , 5)
    expect_equal(
        length(scale.dispRity(disparity2))
        , 5)
    expect_equal(
        length(scale.dispRity(disparity3))
        , 5)
    expect_true(
        max(unlist(extract.dispRity(scale.dispRity(disparity1)))) <= 1
        )
    expect_true(
        max(unlist(extract.dispRity(scale.dispRity(disparity2)))) <= 1
        )
    expect_true(
        max(unlist(extract.dispRity(scale.dispRity(disparity3)))) <= 1
        )
})
