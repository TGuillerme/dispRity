# TESTING summary.dispRity

context("summary.dispRity")
 
# Loading the data
load("test_data.Rda")
data<-test_data$ord_data_tips

#######################
#Testing
#######################

#Case 1, no bootstrap
data <- test_data$ord_data_tips
data <- dispRity(data, metric = c(sum, ranges))
test <- summary(data)

#Test
test_that("Works without bootstraps", {
    expect_is(
        test, "data.frame"
        )
    expect_equal(
        nrow(test), 1
        )
    expect_equal(
        ncol(test), 3
        )
})

#Case 2, bootstraps
data <- test_data$ord_data_tips
data <- boot.matrix(data, bootstrap = 5)
data <- dispRity(data, metric = c(sum, ranges))
test <- summary(data)

#Test
test_that("Works with bootstraps", {
    expect_is(
        test, "data.frame"
        )
    expect_equal(
        nrow(test), 1
        )
    expect_equal(
        ncol(test), 8
        )
})

#Case 3, bootstraps + rarefaction
data <- test_data$ord_data_tips
data <- boot.matrix(data, bootstrap = 5, rarefaction = c(5,50))
data <- dispRity(data, metric = c(sum, ranges))
test <- summary(data)

#Test
test_that("Works with bootstraps and rarefaction", {
    expect_is(
        test, "data.frame"
        )
    expect_equal(
        nrow(test), 2
        )
    expect_equal(
        ncol(test), 8
        )
    expect_equal(
        test$obs
        , c(45.36, NA))
})

#Case 4, time subsamples
data <- test_data$ord_data_tips
factor <- as.data.frame(matrix(data = c(rep(1, nrow(data)/2),rep(2, nrow(data)/2)), nrow = nrow(data), ncol = 1))
rownames(factor) <- rownames(data)
data <- custom.subsamples(data, factor)
data <- dispRity(data, metric = c(sum, ranges))
test <- summary(data)

#Test
test_that("Works with subsamples", {
    expect_is(
        test, "data.frame"
        )
    expect_equal(
        nrow(test), 2
        )
    expect_equal(
        ncol(test), 3
        )
    expect_equal(
        test$obs
        ,c(37.00, 37.97))
})

#Case 5, time subsamples + bootstraps
set.seed(1)
data <- test_data$ord_data_tips
factor <- as.data.frame(matrix(data = c(rep(1, nrow(data)/2),rep(2, nrow(data)/2)), nrow = nrow(data), ncol = 1))
rownames(factor) <- rownames(data)
data <- custom.subsamples(data, factor)
data <- boot.matrix(data, bootstrap = 5)
data <- dispRity(data, metric = c(sum, ranges))
test <- summary(data)

#Test
test_that("Works with subsamples and bootstraps", {
    expect_is(
        test, "data.frame"
        )
    expect_equal(
        nrow(test), 2
        )
    expect_equal(
        ncol(test), 8
        )
    expect_equal(
        test$bs.median
        ,c(32.65, 34.09))
})

#Case 5, time subsamples + bootstraps + rarefaction
set.seed(1)
data <- test_data$ord_data_tips
factor <- as.data.frame(matrix(data = c(rep(1, nrow(data)/2),rep(2, nrow(data)/2)), nrow = nrow(data), ncol = 1))
rownames(factor) <- rownames(data)
data <- custom.subsamples(data, factor)
data <- boot.matrix(data, bootstrap = 5, rarefaction = c(5,6))
data <- dispRity(data, metric = c(sum, ranges))
test <- summary(data)

#Test
test_that("Works with subsamples, bootstraps and rarefaction", {
    expect_is(
        test, "data.frame"
        )
    expect_equal(
        nrow(test), 6
        )
    expect_equal(
        ncol(test), 8
        )
    expect_equal(
        test$obs
        , c(37.00, NA, NA, 37.97, NA, NA))
    expect_equal(
        test$bs.median
        , c(32.65, 20.39, 21.86, 33.75, 21.44, 23.33))
})

#Example
test_that("Example works", {
    factors <- as.data.frame(matrix(data = c(rep(1, nrow(BeckLee_mat50)/2), rep(2, nrow(BeckLee_mat50)/2)), nrow = nrow(BeckLee_mat50), ncol = 1, dimnames = list(rownames(BeckLee_mat50))))
    customised_subsamples <- custom.subsamples(BeckLee_mat50, factors)
    bootstrapped_data <- boot.matrix(customised_subsamples, bootstraps=100)
    sum_of_ranges <- dispRity(bootstrapped_data, metric=c(sum, ranges))
    expect_is(
        summary(sum_of_ranges), "data.frame"
        )
    expect_equal(
        dim(summary(sum_of_ranges)), c(2,8)
        )
    expect_is(
        summary(sum_of_ranges, quantile=75, cent.tend=median, rounding=0), "data.frame"
        )
    expect_equal(
        dim(summary(sum_of_ranges, quantile=75, cent.tend=median, rounding=0)), c(2,6)
        )
})

#Testing with distributions
test_that("Test with disparity as a distribution", {
    factors <- as.data.frame(matrix(data = c(rep(1, nrow(BeckLee_mat50)/2), rep(2, nrow(BeckLee_mat50)/2)), nrow = nrow(BeckLee_mat50), ncol = 1, dimnames = list(rownames(BeckLee_mat50))))
    customised_subsamples <- custom.subsamples(BeckLee_mat50, factors)
    sum_of_ranges1 <- dispRity(customised_subsamples, metric=ranges)
    bootstrapped_data <- boot.matrix(customised_subsamples, bootstraps=100)
    sum_of_ranges2 <- dispRity(bootstrapped_data, metric=ranges)

    expect_is(
        summary(sum_of_ranges1), "data.frame"
        )
    expect_is(
        summary(sum_of_ranges2), "data.frame"
        )

    expect_equal(
        dim(summary(sum_of_ranges1))
        , c(2,7))
    expect_equal(
        dim(summary(sum_of_ranges2)), c(2,8)
        )
})


# test_that("Test seq.test object management", {
#     data(BeckLee_mat50)
#     factors <- as.data.frame(matrix(data = c(rep(1, 12), rep(2, 13), rep(3, 12), rep(4, 13)), dimnames = list(rownames(BeckLee_mat50))), ncol = 1)
#     customised_subsamples <- custom.subsamples(BeckLee_mat50, factors)
#     bootstrapped_data <- boot.matrix(customised_subsamples, bootstraps = 3)
#     sum_of_variances <- dispRity(bootstrapped_data, metric =  variances)
#     data_distribution <- sequential.test(extract.dispRity(sum_of_variances, observed = FALSE, keep.structure = TRUE, concatenate = FALSE), family = gaussian)
#     data_concatenated <- sequential.test(extract.dispRity(sum_of_variances, observed = FALSE, keep.structure = TRUE, concatenate = TRUE), family = gaussian)

#     expect_is(
#         summary(data_distribution), "list"
#         )
#     expect_is(
#         summary(data_concatenated), "list"
#         )

#     expect_equal(
#         length(summary(data_distribution))
#         ,2)
#     expect_equal(
#         length(summary(data_concatenated))
#         ,2)

#     expect_equal(
#         names(summary(data_distribution))
#         ,c("Slopes","Intercepts"))
#     expect_equal(
#         names(summary(data_concatenated))
#         ,c("Slopes","Intercepts"))

#     #concatenated results are two matrices
#     expect_equal(
#         unique(unlist(lapply(summary(data_concatenated), class)))
#         ,"matrix")

#     #distribution results are two lists...
#     expect_equal(
#         unique(unlist(lapply(summary(data_distribution), class)))
#         ,"list")

#     #... of matrices
#     expect_equal(
#         unique(unlist(lapply(summary(data_distribution), lapply, class)))
#         ,"matrix")
# })