# TESTING dispRity

context("dispRity")

data(BeckLee_mat50)
data(BeckLee_tree)
data_boot <- boot.matrix(BeckLee_mat50, bootstraps = 11, rarefaction = c(5,6))
data_subsets_simple <- chrono.subsets(BeckLee_mat50, tree = BeckLee_tree,  method = "discrete", time = c(120,80,40,20))
data_subsets_boot <- boot.matrix(data_subsets_simple, bootstraps = 11, rarefaction = c(5,6))
metric = c(sum, variances)
verbose = TRUE
data <- data_subsets_boot


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
    one_bs_matrix <- data$subsets[[1]][[5]]
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
    test0 <- disparity.bootstraps.silent(data$subsets[[1]][[2]], metrics_list, data, matrix_decomposition)
    expect_message(test0v <- disparity.bootstraps.verbose(data$subsets[[1]][[2]], metrics_list, data, matrix_decomposition))

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
    test1 <- disparity.bootstraps.silent(data$subsets[[1]][[2]], metrics_list, data, matrix_decomposition)
    expect_message(test1v <- disparity.bootstraps.verbose(data$subsets[[1]][[2]], metrics_list, data, matrix_decomposition))

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
    test <- fill.dispRity(make.dispRity(data = data))
    test$matrix <- NULL
    expect_error(
        dispRity(test, c(sum, ranges))
        )
    test <- dispRity(data, ranges)
    expect_error(
        dispRity(test, c(var, sd))
        )

    ## Dimensions
    expect_error(
        dispRity(data, c(sum, ranges), dimensions = -1)
    )
    expect_equal(
        dispRity(data, c(sum, ranges), dimensions = 0.533333)$call$dimensions
        , 26
    )

    warn <- capture_warnings(dim <- dispRity(data, c(sum, ranges), dimensions = 100)$call$dimensions)
    expect_equal(dim, 48)
    expect_equal(warn , "Dimension number too high: set to 48.")


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


#one matrix with subsets
data<-test_data$ord_data_tips
data<-custom.subsets(data, group)
test<-dispRity(data, metric=c(sum, ranges))
test_that("dispRity works with custom subsets", {
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

#bootstrapped + rarefied + subsets
group<-as.data.frame(matrix(data=c(rep(1, nrow(data)/2),rep(2, nrow(data)/2)), nrow=nrow(data), ncol=1))
rownames(group)<-rownames(data)
data<-custom.subsets(data, group)
data<-boot.matrix(data, bootstrap=5, rarefaction=FALSE, boot.type="full")
test<-dispRity(data, metric=c(sum, ranges))

test_that("dispRity works with a bootstrapped, rarefied, custom subsets", {
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


#testing example
test_that("Example works", {
    data(BeckLee_mat50)
    sum_of_ranges <- dispRity(BeckLee_mat50, metric = c(sum, ranges))
    ex1<-summary(sum_of_ranges)
    expect_is(
        ex1, "data.frame"
        )
    expect_equal(
        dim(ex1), c(1,3)
        )

    bootstrapped_data <- boot.matrix(BeckLee_mat50, bootstraps=100)
    ex2<-dispRity(bootstrapped_data, metric=c(sum, ranges))
    expect_is(
        ex2, "dispRity"
        )
    expect_equal(
        dim(ex2[[1]]), c(50,48)
        )

    groups <- as.data.frame(matrix(data = c(rep(1, nrow(BeckLee_mat50)/2), rep(2, nrow(BeckLee_mat50)/2)), nrow = nrow(BeckLee_mat50), ncol = 1, dimnames = list(rownames(BeckLee_mat50))))
    customised_subsets <- custom.subsets(BeckLee_mat50, groups)
    set.seed(1)
    bootstrapped_data <- boot.matrix(customised_subsets, bootstraps=100)
    sum_of_ranges <- dispRity(bootstrapped_data, metric=c(sum, ranges))

    ex3 <- summary(sum_of_ranges, digits = 2)

    expect_is(
        ex3
        , "data.frame")
    expect_equal(
        dim(ex3)
        , c(2,8))

    ranges <- dispRity(BeckLee_mat50, metric = ranges)
    sum_of_ranges <- dispRity(ranges, metric = sum)
    ex1<-summary(sum_of_ranges)
    expect_is(
        ex1, "data.frame"
        )
    expect_equal(
        dim(ex1), c(1,3)
        )

    
})
#Reset
test <- NULL ; data<-test_data$ord_data_tips

## dispRity works with empty or small (<3 subsets)
test_that("dispRity works with small, empty/subsets", {

    load("test_data.Rda")
    tree <- test_data$tree_data
    data <- test_data$ord_data_tips_nodes
    FADLAD <- test_data$FADLAD_data

    silent <- capture_warnings(data <- chrono.subsets(data, tree, model = "deltran", method = "continuous", time = c(140, 138, 130, 120, 100)))
    silent <- capture_warnings(data <- boot.matrix(data))

    warnings <- capture_warnings(test <- dispRity(data, metric = c(sum, variances), verbose = TRUE))

    expect_equal(warnings, "Disparity not calculated for subsets 140, 138 (not enough data).")

    expect_equal(test$disparity[[1]][[1]][,1], NA)
    expect_equal(test$disparity[[1]][[2]][,1], NA)
    expect_equal(test$disparity[[2]][[1]][,1], NA)
    expect_equal(test$disparity[[2]][[2]][,1], NA)
})


test_that("dispRity deals with probabilities subsets", {
    data(BeckLee_mat99)
    data(BeckLee_ages)
    data(BeckLee_tree)
    

    data1 <- chrono.subsets(BeckLee_mat99, BeckLee_tree, method = "continuous", time = c(100, 60), model = "gradual.split", inc.nodes = TRUE, BeckLee_ages, verbose = FALSE, t0 = FALSE)
    data2 <- chrono.subsets(BeckLee_mat99, BeckLee_tree, method = "continuous", time = c(100, 60), model = "proximity", inc.nodes = TRUE, BeckLee_ages, verbose = FALSE, t0 = FALSE)

    set.seed(1)
    test1 <- dispRity(data1, metric = mean)
    set.seed(1)
    test2 <- dispRity(data2, metric = mean)
    set.seed(2)
    test3 <- dispRity(data1, metric = mean)

    expect_equal(summary(test1)$n, c(15,21))
    expect_equal(summary(test2)$n, c(11,20))
    expect_equal(summary(test3)$n, c(15,21))

    expect_equal(as.vector(summary(test1)$obs), c(-0.010, 0.007))
    expect_equal(as.vector(summary(test2)$obs), c(-0.012, 0.004))
    expect_equal(as.vector(summary(test3)$obs), c(-0.006, 0.007))

})



# test_that("dispRity works in parallel", {
#     library(parallel)
#     data(BeckLee_mat99)
#     test <- boot.matrix(BeckLee_mat99, 100)

#     ## Control
#     test_non_par <- dispRity(test, metric = c(sum, variances))

#     ## Error
#     expect_error(dispRity(test, metric = c(sum, variances), parallel = "whatever"))

#     ## Run parallel
#     test_par <- dispRity(test, metric = c(sum, variances), parallel = TRUE)

# })