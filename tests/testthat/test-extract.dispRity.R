# #TESTING dispRity

# context("extract.dispRity")

# ## Calculating some disparity
# data(BeckLee_mat99) ; data(BeckLee_tree)
# suppressMessages(subsamples <- time.subsamples(BeckLee_mat99, BeckLee_tree, method = "continuous", time = c(100,80,60), model = "gradual"))
# bootstraps_dat <- boot.matrix(subsamples, bootstraps = 20, rarefaction = TRUE)
# disparity_data <- dispRity(bootstraps_dat, metric = mean)

# test_that("Example works", {
#     ex1 <- lapply(extract.dispRity(disparity_data), round, digit=5)
#     expect_equal(
#     	length(ex1), 3
#     	)
#     expect_equal(
#     	names(ex1), c("100","80", "60")
#     	)
#     expect_equal(
#     	as.vector(unlist(ex1)), c(-0.01373,-0.00340,0.00453)
#     	)
#     ex2 <- extract.dispRity(disparity_data, observed = FALSE)
#     expect_is(
#     	ex2, "list"
#     	)
#     expect_equal(
#     	length(ex2), 3
#     	)
#     expect_equal(
#     	names(ex2), c("100","80", "60")
#     	)
#     expect_equal(
#     	ex2[[1]],unlist(disparity_data$disparity$bootstrapped[[1]][[8]])
#     	)
#     ex3 <- extract.dispRity(disparity_data, observed = FALSE, rarefaction = "min")
#     expect_is(
#     	ex3, "list"
#     	)
#     expect_equal(
#     	length(ex3), 3
#     	)
#     expect_equal(
#     	names(ex3), c("100","80", "60")
#     	)
#     expect_equal(
#     	ex3[[1]],unlist(disparity_data$disparity$bootstrapped[[1]][[1]])
#     	)
#     ex4 <- extract.dispRity(disparity_data, observed = FALSE, rarefaction = 5)
#     expect_is(
#     	ex4, "list"
#     	)
#     expect_equal(
#     	length(ex4), 3
#     	)
#     expect_equal(
#     	names(ex4), c("100","80", "60")
#     	)
#     expect_equal(
#     	ex4[[1]],unlist(disparity_data$disparity$bootstrapped[[1]][[3]])
#         )    
# })


# disparity_data <- dispRity(bootstraps_dat, metric = ranges)
# test_that("Works with level2 outputs", {
#     ex1 <- extract.dispRity(disparity_data, observed = TRUE)
#     expect_is(
#         ex1, "list"
#         )
#     expect_equal(
#         names(ex1), c("100","80", "60")
#         )
#     expect_equal(
#         length(ex1[[1]]), ncol(bootstraps_dat$data[[1]][[1]][[1]][[1]])
#         )
#     ex2 <- extract.dispRity(disparity_data, observed = FALSE)
#     expect_is(
#         ex2, "list"
#         )
#     expect_equal(
#         names(ex2), c("100","80", "60")
#         ) 
#     expect_equal(
#         length(ex2[[1]]), 1940
#         )
#     ex3 <- extract.dispRity(disparity_data, observed = FALSE, concatenate = FALSE)
#     expect_is(
#         ex3, "list"
#         )
#     expect_equal(
#         names(ex3), c("100","80", "60")
#         ) 
#     expect_equal(
#         length(ex3[[1]]), 20
#         )
#     expect_equal(
#         length(ex3[[1]][[1]]), 97
#         )  
# })