context("sort.dispRity")


# Example data
data(BeckLee_mat50)
factors <- as.data.frame(matrix(data = c(rep(1, nrow(BeckLee_mat50)/2), rep(2, nrow(BeckLee_mat50)/2)), nrow = nrow(BeckLee_mat50), ncol = 1, dimnames = list(rownames(BeckLee_mat50))))
customised_series <- cust.series(BeckLee_mat50, factors)
bootstrapped_data <- boot.matrix(customised_series, bootstraps = 7, rarefaction = c(10, 25))
disparity1 <- dispRity(BeckLee_mat50, metric = c(sum, centroids))
disparity2 <- dispRity(customised_series, metric = c(sum, centroids))
disparity3 <- dispRity(bootstrapped_data, metric = c(sum, centroids))


#Test
test_that("sort.dispRity works", {
    #class - single
    expect_error(
        sort.dispRity("bla")
        )
    expect_error(
        sort.dispRity(matrix(rnorm(10)))
        )
    expect_error(
        sort.dispRity(disparity1)
        )
    #returns the right objects
    expect_is(
        sort.dispRity(disparity2)
        , "dispRity")
    expect_is(
        sort.dispRity(disparity3)
        , "dispRity")
    expect_equal(
        length(sort.dispRity(disparity2))
        , 5)
    expect_equal(
        length(sort.dispRity(disparity3))
        , 5)
    expect_equal(
        sort.dispRity(disparity2)$series
        , c("V1.1", "V1.2"))
    expect_equal(
        sort.dispRity(disparity2, decreasing = TRUE)$series
        , c("V1.2", "V1.1"))
    expect_equal(
        sort.dispRity(disparity2, sort = c(2,1))$series
        , c("V1.2", "V1.1"))
})
