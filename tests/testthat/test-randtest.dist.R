## Test
test_that("randtest.dist works", {

    set.seed(1)
    dummy_matrix <- matrix(rnorm(500), 100, 5)
    dummy_matrix[c(1,2,3), ] <- rep(1, 5)
    dummy_matrix[c(4,5,6), ] <- rep(-1, 5)
    test <- randtest.dispRity(dummy_matrix,
                              subsets = sample(1:100, 20),
                              metric = mean)
    test_right <- randtest.dispRity(dummy_matrix,
                              subsets = c(1,2,3),
                              metric = mean)
    test_left <- randtest.dispRity(dummy_matrix,
                              subsets = c(4,5,6),
                              metric = mean)
    
    quant <- c(0.4, 0.6)

    ## Sanitizing
    error <- capture_error(randtest.dist("test", quantile = quant, abs = TRUE))
    expect_equal(error[[1]], "randtest must be of class randtest.")
    error <- capture_error(randtest.dist(test, quantile = "quant", abs = TRUE))
    expect_equal(error[[1]], "quantile must be of class numeric.")
    error <- capture_error(randtest.dist(test, quantile = quant, abs = "Wrong!"))
    expect_equal(error[[1]], "abs must be of class logical.")

    res <- randtest.dist(test)
    expect_equal_round(res, c("2.5%" = -0.2861862), digits = 6)
    res <- randtest.dist(test, abs = TRUE)
    expect_equal_round(res, c("2.5%" = 0.2861862), digits = 6)
    res <- randtest.dist(test_right, quantile = quant)
    expect_equal_round(res, c("60%" = 0.9972712), digits = 6)
    res <- randtest.dist(test_left, quantile = quant)
    expect_equal_round(res, c("40%" = 0.7982932), digits = 6)
})
