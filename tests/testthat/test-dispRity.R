# TESTING dispRity

context("dispRity")

data(BeckLee_mat50)
data(BeckLee_tree)
data_boot <- boot.matrix(BeckLee_mat50, bootstraps = 11, rarefaction = c(5,6))
data_subsamples_simple <- time.subsamples(BeckLee_mat50, tree = BeckLee_tree,  method = "discrete", time = c(120,80,40,20))
data_subsamples_boot <- boot.matrix(data_subsamples_simple, bootstraps = 11, rarefaction = c(5,6))
metric = c(sum, variances)
verbose = TRUE
data <- data_subsamples_boot


test_that("get.dispRity.metric.handle", {
    match_call <- list("data" = NA, "metric" = NA, "verbose" = FALSE)
    
    ## Level1
    test <- get.dispRity.metric.handle(sum, match_call)
    expect_is(test, "list")
    expect_null(test[[1]])
    expect_null(test[[2]])
    expect_is(test[[3]], "function")

    ## Level2
    test <- get.dispRity.metric.handle(ranges, match_call)
    expect_is(test, "list")
    expect_null(test[[1]])
    expect_is(test[[2]], "function")
    expect_null(test[[3]])

    ## Level3
    expect_error(test <- get.dispRity.metric.handle(var, match_call))
    test <- get.dispRity.metric.handle(c(sd, var), match_call)
    expect_is(test, "list")
    expect_is(test[[1]], "function")
    expect_null(test[[2]])
    expect_is(test[[3]], "function")

})


test_that("get.first.metric", {
    test1 <- get.dispRity.metric.handle(c(sd, var), match_call)
    test2 <- get.dispRity.metric.handle(variances, match_call)
    test3 <- get.dispRity.metric.handle(sd, match_call)

    ## Metrics output is 1: function, 2: list -1 and 3: level
    ## Remove a level 1
    res_test1 <- get.first.metric(test1)
    expect_is(res_test1, "list")
    expect_equal(length(res_test1), 3)
    expect_is(res_test1[[1]], "function")
    expect_null(res_test1[[2]][[1]])
    expect_equal(res_test1[[3]], 1)

    ## Removing a level 2
    res_test2 <- get.first.metric(test2)
    expect_is(res_test2, "list")
    expect_equal(length(res_test2), 3)
    expect_is(res_test2[[1]], "function")
    expect_null(res_test2[[2]][[2]])
    expect_equal(res_test2[[3]], 2)

    ## Removing a level 3
    res_test3 <- get.first.metric(test3)
    expect_is(res_test3, "list")
    expect_equal(length(res_test3), 3)
    expect_is(res_test3[[1]], "function")
    expect_null(res_test3[[2]][[3]])
    expect_equal(res_test3[[3]], 3)
})

test_that("apply.decompose.matrix", {
    data(disparity)
    ## Shortening the matrix for the example
    data <- disparity
    data$call$dimensions <- 7
    one_bs_matrix <- data$subsamples[[1]][[5]]
    bs_max <- 5

    decomp_array <- apply.decompose.matrix(one_bs_matrix[,1:bs_max], fun = variances, data = data, use_array = TRUE)
    decomp_matrix <- apply.decompose.matrix(one_bs_matrix[,1:bs_max], fun = variances, data = data, use_array = FALSE)

    expect_is(decomp_array, "array")
    expect_equal(dim(decomp_array), c(data$call$dimensions, data$call$dimensions, bs_max))
    expect_is(decomp_matrix, "matrix")
    expect_equal(dim(decomp_matrix), c(data$call$dimensions, bs_max))
})


test_that("disparity.bootstraps internal works", {
    data(BeckLee_mat50)
    data <- boot.matrix(BeckLee_mat50, bootstraps = 11, rarefaction = c(5,6))
    metrics_list <- list("level3.fun" = var, "level2.fun" = NULL, "level1.fun" = NULL)
    matrix_decomposition = TRUE
    ## With matrix decomposition
    test0 <- disparity.bootstraps.silent(data$subsamples[[1]][[2]], metrics_list, data, matrix_decomposition)
    expect_message(test0v <- disparity.bootstraps.verbose(data$subsamples[[1]][[2]], metrics_list, data, matrix_decomposition))

    ## Should be a array
    expect_is(
        test0
        , "array")
    ## Should be of dimensions 48 * 48 * 11
    expect_equal(
        dim(test0)
        , c(48, 48, 11))
    ## which should be recorded by the way
    expect_equal(
        dim(test0)
        ,as.numeric(c(data$call$dimensions,data$call$dimensions, data$call$bootstrap[[1]])))
    ## test0 and test0v are the same
    expect_true(all(test0 == test0v))

    metrics_list <- list("level3.fun" = NULL, "level2.fun" = variances, "level1.fun" = NULL)
    matrix_decomposition = TRUE
    ## With matrix decomposition
    test1 <- disparity.bootstraps.silent(data$subsamples[[1]][[2]], metrics_list, data, matrix_decomposition)
    expect_message(test1v <- disparity.bootstraps.verbose(data$subsamples[[1]][[2]], metrics_list, data, matrix_decomposition))

    ## Should be a matrix
    expect_is(
        test1
        , "matrix")
    ## Should be of dimensions 48 * 11
    expect_equal(
        dim(test1)
        , c(48, 11))
    ## which should be recorded by the way
    expect_equal(
        dim(test1)
        ,as.numeric(c(data$call$dimensions, data$call$bootstrap[[1]])))
    ## test1 and test1v are the same
    expect_true(all(test1 == test1v))

    ## Without matrix decomposition
    matrix_decomposition = FALSE
    metrics_list <- list("level3.fun" = NULL, "level2.fun" = NULL, "level1.fun" = mean)
    test2 <- disparity.bootstraps.silent(test1, metrics_list, data, matrix_decomposition)
    expect_message(test2v <- disparity.bootstraps.verbose(test1, metrics_list, data, matrix_decomposition))

    ## Should be a matrix
    expect_is(
        test2
        , "matrix")
    ## Should be of dimensions 1 * 11
    expect_equal(
        dim(test2)
        , c(1, 11))
    ## test2 and test2v are the same
    expect_true(all(test2 == test2v))
})


#Loading the data
load("test_data.Rda")
data <- test_data$ord_data_tips
group <- as.data.frame(matrix(data = c(rep(1, nrow(data)/2),rep(2, nrow(data)/2)), nrow = nrow(data), ncol = 1))
rownames(group) <- rownames(data)

#Sanitizing
test_that("Sanitizing works", {
    expect_error(
        dispRity(data="a", c(sum, ranges), FALSE)
        )
    expect_error(
        dispRity(data=1, c(sum, ranges), FALSE)
        )
    expect_error(
        dispRity(data, metric="a", FALSE)
        )
    expect_error(
        dispRity(data, metric=1, FALSE)
        )
    expect_error(
        dispRity(data, metric=c(1,2), FALSE)
        )
    expect_error(
        dispRity(data, metric=c(sum, data), FALSE)
        )
    expect_error(
        dispRity(data, c(sum, ranges), verbose="yes")
        )
})
#Reset
test <- NULL ; data<-test_data$ord_data_tips

#Testing metric argument
test_that("metric argument works", {
    expect_error(
        dispRity(BeckLee_mat50, metric = var)
    )
    expect_error(
        dispRity(BeckLee_mat50, metric = c(var))
    )
    expect_is(
        dispRity(BeckLee_mat50, metric = c(var, sd))
        , "dispRity")
    expect_is(
        dispRity(BeckLee_mat50, metric = c(var, variances, sd))
        , "dispRity")
    expect_is(
        dispRity(BeckLee_mat50, metric = sd)
        , "dispRity")
    expect_is(
        dispRity(BeckLee_mat50, metric = c(sd))
        , "dispRity")
    expect_error(
        dispRity(BeckLee_mat50, metric = c(sd, sd))
        )
    expect_error(
        dispRity(BeckLee_mat50, metric = c(var, sd, sd))
        )
})

#one matrix
test <- dispRity(data, metric=c(sum, ranges))

test_that("dispRity works with a single matrix", {
    expect_is(
        test, "dispRity"
        )
    expect_equal(
        length(test$disparity)
        , 1)
    expect_equal(
        length(test$disparity[[1]])
        , 1)
    expect_is(
        test$disparity[[1]][[1]]
        , "matrix")
})

#Reset
test <- NULL ; data<-test_data$ord_data_tips

#bootstrapped
data<-boot.matrix(data, bootstrap=5, rarefaction=FALSE, boot.type="full")
test<-dispRity(data, metric=c(sum, ranges))
test_that("dispRity works with a bootstrapped matrix", {
    expect_is(
        test, "dispRity"
        )
    expect_equal(
        length(test$disparity)
        , 1)
    expect_equal(
        length(test$disparity[[1]])
        , 2)
    expect_is(
        test$disparity[[1]][[2]]
        , "matrix")
    expect_length(
        test$disparity[[1]][[2]]
        , 5)
    expect_length(
        test$disparity[[1]][[2]]
        , test$call$bootstrap[[1]])
})
#Reset
test <- NULL ; data<-test_data$ord_data_tips

#bootstrapped + rarefied
data<-test_data$ord_data_tips
data<-boot.matrix(data, bootstrap=5, rarefaction=TRUE, boot.type="full")
test<-dispRity(data, metric=c(sum, ranges))
test_that("dispRity works with a bootstrapped and rarefied matrix", {
    expect_is(
        test, "dispRity"
        )
    expect_equal(
        length(test$disparity)
        , 1)
    expect_equal(
        length(test$disparity[[1]])
        , 49)
    for(rare in 2:49) {
        expect_is(
            test$disparity[[1]][[rare]]
            , "matrix")
        expect_length(
            test$disparity[[1]][[rare]]
            , test$call$bootstrap[[1]])
    }
})
#Reset
test <- NULL ; data<-test_data$ord_data_tips


#one matrix with subsamples
data<-test_data$ord_data_tips
data<-custom.subsamples(data, group)
test<-dispRity(data, metric=c(sum, ranges))
test_that("dispRity works with custom subsamples", {
    expect_is(
        test, "dispRity"
        )
    expect_equal(
        length(test$disparity)
        , 2)
    expect_equal(
        length(test$disparity[[1]])
        , 1)
    expect_is(
        test$disparity[[1]][[1]]
        , "matrix")
})
#Reset
test <- NULL ; data<-test_data$ord_data_tips

#bootstrapped + rarefied + subsamples
group<-as.data.frame(matrix(data=c(rep(1, nrow(data)/2),rep(2, nrow(data)/2)), nrow=nrow(data), ncol=1))
rownames(group)<-rownames(data)
data<-custom.subsamples(data, group)
data<-boot.matrix(data, bootstrap=5, rarefaction=FALSE, boot.type="full")
test<-dispRity(data, metric=c(sum, ranges))

test_that("dispRity works with a bootstrapped, rarefied, custom subsamples", {
    expect_is(
        test, "dispRity"
        )
    expect_equal(
        length(test$disparity)
        , 2)
    expect_equal(
        length(test$disparity[[1]])
        , 2)
    expect_is(
        test$disparity[[1]][[2]]
        , "matrix")
    expect_length(
        test$disparity[[1]][[2]]
        , 5)
})
#Reset
test <- NULL ; data<-test_data$ord_data_tips




# #testing example
# test_that("Example works", {
#     data(BeckLee_mat50)
#     sum_of_ranges <- dispRity(BeckLee_mat50, metric = c(sum, ranges))
#     ex1<-summary(sum_of_ranges)
#     expect_is(
#         ex1, "data.frame"
#         )
#     expect_equal(
#         dim(ex1), c(1,3)
#         )

#     bootstrapped_data <- boot.matrix(BeckLee_mat50, bootstraps=100)
#     ex2<-dispRity(bootstrapped_data, metric=c(sum, ranges))
#     expect_is(
#         ex2, "dispRity"
#         )
#     expect_equal(
#         dim(ex2[[1]][[1]][[1]][[1]][[1]]), c(50,48)
#         )

#     groups <- as.data.frame(matrix(data = c(rep(1, nrow(BeckLee_mat50)/2), rep(2, nrow(BeckLee_mat50)/2)), nrow = nrow(BeckLee_mat50), ncol = 1, dimnames = list(rownames(BeckLee_mat50))))
#     customised_subsamples <- custom.subsamples(BeckLee_mat50, groups)
#     set.seed(1)
#     bootstrapped_data <- boot.matrix(customised_subsamples, bootstraps=100)
#     sum_of_ranges <- dispRity(bootstrapped_data, metric=c(sum, ranges))

#     ex3 <- summary(sum_of_ranges, rounding = 2)

#     expect_is(
#         ex3
#         , "data.frame")
#     expect_equal(
#         dim(ex3)
#         , c(2,8))
#     expect_equal(
#         sum(ex3[,4])
#         , sum(c(32.67,33.85)))
# })
# #Reset
# test <- NULL ; data<-test_data$ord_data_tips

# #testing with distributions as output (level2 functions outputs)
# test_that("dispRity works with level2 functions", {
#     data(BeckLee_mat50)
#     ranges_test <- dispRity(BeckLee_mat50, metric = ranges)
#     #Output is a distribution of ncol(data) ranges
#     expect_equal(
#         ncol(ranges_test$data[[1]][[1]]), length(ranges_test$disparity$observed[[1]][[1]][[1]])
#         )
#     #Output is a distribution of ncol(data) ranges (works for bootstraps as well)
#     ranges_test <- dispRity(boot.matrix(BeckLee_mat50, 2), metric = ranges)
#     expect_equal(
#         ncol(ranges_test$data[[1]][[1]][[1]][[1]]), length(ranges_test$disparity$bootstrapped[[1]][[1]][[1]])
#         )
#     expect_equal(
#         ncol(ranges_test$data[[1]][[1]][[1]][[1]]), length(ranges_test$disparity$bootstrapped[[1]][[1]][[2]])
#         )
# })

# #testing with disparity inputs
# test_that("dispRity works with disparity inputs", {
#     ranges_test <- dispRity(data, metric = ranges)
#     set.seed(1)
#     ranges_test_bs <- dispRity(boot.matrix(data, 10), metric = ranges)
#     ranges_test_subsamples <- dispRity(custom.subsamples(data, group), metric = ranges)
#     set.seed(1)
#     ranges_test_subsamples_bs <- dispRity(boot.matrix(custom.subsamples(data, group), 10), metric = ranges)

#     mean_ranges_test <- dispRity(ranges_test, metric = mean)
#     set.seed(1)
#     mean_ranges_test_bs <- dispRity(ranges_test_bs, metric = mean)
#     mean_ranges_test_subsamples <- dispRity(ranges_test_subsamples, metric = mean)
#     set.seed(1)
#     mean_ranges_test_subsamples_bs <- dispRity(ranges_test_subsamples_bs, metric = mean)

#     #The calculated mean in mean_ranges_test must be equal to the mean in range_test summary
#     expect_equal(
#         summary(ranges_test)[1,3], summary(mean_ranges_test)[1,3]
#         )
#     expect_equal(
#         summary(ranges_test_bs)[1,3], summary(mean_ranges_test_bs)[1,3]
#         )
#     expect_equal(
#         summary(ranges_test_subsamples)[c(1:2),3], summary(mean_ranges_test_subsamples)[c(1:2),3]
#         )
#     expect_equal(
#         summary(ranges_test_subsamples_bs)[c(1:2),3], summary(mean_ranges_test_subsamples_bs)[c(1:2),3]
#         )

#     #Same for the bs values
#     expect_equal(
#         summary(ranges_test_bs)[1,4], summary(mean_ranges_test_bs)[1,4]
#         )
#     expect_equal(
#         summary(ranges_test_subsamples_bs)[c(1:2),4], summary(mean_ranges_test_subsamples_bs)[c(1:2),4]
#         )
# })