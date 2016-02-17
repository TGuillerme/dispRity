#TESTING dispRity

context("extract.dispRity")

## Calculating some disparity
data(BeckLee_mat99) ; data(BeckLee_tree)
suppressMessages(series <- time.series(BeckLee_mat99, BeckLee_tree, method = "continuous", time = c(100,80,60), model = "gradual"))
bootstraps_dat <- boot.matrix(series, bootstraps = 20, rarefaction = TRUE)
disparity_data <- dispRity(bootstraps_dat, metric = mean)

test_that("Example works", {
    ex1 <- round(extract.dispRity(disparity_data), digit=5)
    expect_equal(
    	length(ex1), 3
    	)
    expect_equal(
    	names(ex1), c("100","80", "60")
    	)
    expect_equal(
    	as.vector(ex1), c(-0.01373,-0.00340,0.00453)
    	)
    ex2 <- extract.dispRity(disparity_data, observed = FALSE)
    expect_is(
    	ex2, "list"
    	)
    expect_equal(
    	length(ex2), 3
    	)
    expect_equal(
    	names(ex2), c("100","80", "60")
    	)
    expect_equal(
    	ex2[[1]],unlist(disparity_data$disparity$bootstrapped[[1]][[8]])
    	)
    ex3 <- extract.dispRity(disparity_data, observed = FALSE, rarefaction = "min")
    expect_is(
    	ex3, "list"
    	)
    expect_equal(
    	length(ex3), 3
    	)
    expect_equal(
    	names(ex3), c("100","80", "60")
    	)
    expect_equal(
    	ex3[[1]],unlist(disparity_data$disparity$bootstrapped[[1]][[1]])
    	)
    ex4 <- extract.dispRity(disparity_data, observed = FALSE, rarefaction = 5)
    expect_is(
    	ex4, "list"
    	)
    expect_equal(
    	length(ex4), 3
    	)
    expect_equal(
    	names(ex4), c("100","80", "60")
    	)
    expect_equal(
    	ex4[[1]],unlist(disparity_data$disparity$bootstrapped[[1]][[3]]))    
})
