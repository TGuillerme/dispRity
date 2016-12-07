# TESTING dispRity

context("dispRity")

data(BeckLee_mat50)
data(BeckLee_tree)
data_boot <- boot.matrix(BeckLee_mat50, bootstraps = 11, rarefaction = c(5,6))
data_series_simple <- time.series(BeckLee_mat50, tree = BeckLee_tree,  method = "discrete", time = c(120,80,40,20))
data_series_boot <- boot.matrix(data_series_simple, bootstraps = 11, rarefaction = c(5,6))
metric = c(sum, variances)
verbose = TRUE
data <- data_series_boot

test_that("disparity.bootstraps internal works", {
    data(BeckLee_mat50)
    data <- boot.matrix(BeckLee_mat50, bootstraps = 11, rarefaction = c(5,6))
    metrics_list <- list("level3.fun" = var, "level2.fun" = NULL, "level1.fun" = NULL)
    matrix_decomposition = TRUE
    ## With matrix decomposition
    test0 <- disparity.bootstraps(data$series[[1]][[2]], metrics_list, data, matrix_decomposition)

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


    metrics_list <- list("level3.fun" = NULL, "level2.fun" = variances, "level1.fun" = NULL)
    matrix_decomposition = TRUE
    ## With matrix decomposition
    test1 <- disparity.bootstraps(data$series[[1]][[2]], metrics_list, data, matrix_decomposition)

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


    ## Without matrix decomposition
    matrix_decomposition = FALSE
    metrics_list <- list("level3.fun" = NULL, "level2.fun" = NULL, "level1.fun" = mean)
    test2 <- disparity.bootstraps(test1, metrics_list, data, matrix_decomposition)

    ## Should be a matrix
    expect_is(
        test2
        , "matrix")
    ## Should be of dimensions 1 * 11
    expect_equal(
        dim(test2)
        , c(1, 11))
})


#Loading the data
load("test_data.Rda")
data <- test_data$ord_data_tips
factor <- as.data.frame(matrix(data = c(rep(1, nrow(data)/2),rep(2, nrow(data)/2)), nrow = nrow(data), ncol = 1))
rownames(factor) <- rownames(data)

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
        , 50)
    for(rare in 2:50) {
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


#one matrix with series
data<-test_data$ord_data_tips
data<-cust.series(data, factor)
test<-dispRity(data, metric=c(sum, ranges))
test_that("dispRity works with custom series", {
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

#bootstrapped + rarefied + series
factor<-as.data.frame(matrix(data=c(rep(1, nrow(data)/2),rep(2, nrow(data)/2)), nrow=nrow(data), ncol=1))
rownames(factor)<-rownames(data)
data<-cust.series(data, factor)
data<-boot.matrix(data, bootstrap=5, rarefaction=FALSE, boot.type="full")
test<-dispRity(data, metric=c(sum, ranges))

test_that("dispRity works with a bootstrapped, rarefied, custom series", {
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

#     factors <- as.data.frame(matrix(data = c(rep(1, nrow(BeckLee_mat50)/2), rep(2, nrow(BeckLee_mat50)/2)), nrow = nrow(BeckLee_mat50), ncol = 1, dimnames = list(rownames(BeckLee_mat50))))
#     customised_series <- cust.series(BeckLee_mat50, factors)
#     set.seed(1)
#     bootstrapped_data <- boot.matrix(customised_series, bootstraps=100)
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
#     ranges_test_series <- dispRity(cust.series(data, factor), metric = ranges)
#     set.seed(1)
#     ranges_test_series_bs <- dispRity(boot.matrix(cust.series(data, factor), 10), metric = ranges)

#     mean_ranges_test <- dispRity(ranges_test, metric = mean)
#     set.seed(1)
#     mean_ranges_test_bs <- dispRity(ranges_test_bs, metric = mean)
#     mean_ranges_test_series <- dispRity(ranges_test_series, metric = mean)
#     set.seed(1)
#     mean_ranges_test_series_bs <- dispRity(ranges_test_series_bs, metric = mean)

#     #The calculated mean in mean_ranges_test must be equal to the mean in range_test summary
#     expect_equal(
#         summary(ranges_test)[1,3], summary(mean_ranges_test)[1,3]
#         )
#     expect_equal(
#         summary(ranges_test_bs)[1,3], summary(mean_ranges_test_bs)[1,3]
#         )
#     expect_equal(
#         summary(ranges_test_series)[c(1:2),3], summary(mean_ranges_test_series)[c(1:2),3]
#         )
#     expect_equal(
#         summary(ranges_test_series_bs)[c(1:2),3], summary(mean_ranges_test_series_bs)[c(1:2),3]
#         )

#     #Same for the bs values
#     expect_equal(
#         summary(ranges_test_bs)[1,4], summary(mean_ranges_test_bs)[1,4]
#         )
#     expect_equal(
#         summary(ranges_test_series_bs)[c(1:2),4], summary(mean_ranges_test_series_bs)[c(1:2),4]
#         )
# })