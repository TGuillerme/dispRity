# TESTING dispRity

#context("dispRity")

data(BeckLee_mat99)
data(BeckLee_ages)
data(BeckLee_mat50)
data(BeckLee_tree)
data_boot <- boot.matrix(BeckLee_mat50, bootstraps = 11, rarefaction = c(5,6))
data_subsets_simple <- chrono.subsets(BeckLee_mat50, tree = BeckLee_tree,  method = "discrete", time = c(120,80,40,20))
data_subsets_boot <- boot.matrix(data_subsets_simple, bootstraps = 11, rarefaction = c(5,6))
metric = c(sum, variances)
verbose = TRUE
data <- data_subsets_boot

test_that("get.dispRity.metric.handle", {
    data(disparity)

    match_call <- list("data" = NA, "metric" = NA, "verbose" = FALSE)
    data <- disparity
    
    ## Level1
    test <- get.dispRity.metric.handle(sum, match_call, data = data)
    expect_equal(names(test), c("levels", "between.groups", "tree.metrics", "dist.help"))
    test <- test$levels
    expect_is(test, "list")
    expect_null(test[[1]])
    expect_null(test[[2]])
    expect_is(test[[3]], "function")

    ## Level2
    test <- get.dispRity.metric.handle(ranges, match_call, data = data)
    expect_equal(names(test), c("levels", "between.groups", "tree.metrics", "dist.help"))
    test <- test$levels
    expect_is(test, "list")
    expect_null(test[[1]])
    expect_is(test[[2]], "function")
    expect_null(test[[3]])

    ## Level3
    expect_error(test <- get.dispRity.metric.handle(var, match_call, data = data))
    test <- get.dispRity.metric.handle(c(sd, var), match_call, data = data)
    expect_equal(names(test), c("levels", "between.groups", "tree.metrics", "dist.help"))
    test <- test$levels
    expect_is(test, "list")
    expect_is(test[[1]], "function")
    expect_null(test[[2]])
    expect_is(test[[3]], "function")

    ## Serial
    test.between.groups <- function(matrix, matrix2) {return(42)}
    test.between.groups.no <- function(matrix, matrix3) {return(42)}
    test <- get.dispRity.metric.handle(test.between.groups, match_call, data = data)
    expect_equal(test$between.groups, c(FALSE, FALSE, TRUE))
    test <- get.dispRity.metric.handle(test.between.groups.no, match_call, data = data)
    expect_equal(test$between.groups, c(FALSE, FALSE, FALSE))
})

test_that("get.first.metric", {
    test1 <- get.dispRity.metric.handle(c(sd, var), match_call)$levels
    test2 <- get.dispRity.metric.handle(variances, match_call)$levels
    test3 <- get.dispRity.metric.handle(sd, match_call)$levels

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

test_that("decompose.matrix.wrapper", {
    data(disparity)
    ## Shortening the matrix for the example
    data <- disparity
    data$call$dimensions <- 7
    one_bs_matrix <- data$subsets[[1]][[5]]
    bs_max <- 5

    decomp_array  <- decompose.matrix.wrapper(one_bs_matrix[,1:bs_max], fun = variances, data = data, use_array = TRUE)
    decomp_matrix <- decompose.matrix.wrapper(one_bs_matrix[,1:bs_max], fun = variances, data = data, use_array = FALSE)

    expect_is(decomp_array, "array")
    expect_equal(dim(decomp_array), c(length(data$call$dimensions), length(data$call$dimensions), bs_max))
    expect_is(decomp_matrix, "matrix")
    expect_equal(dim(decomp_matrix), c(length(data$call$dimensions), bs_max))
})

test_that("disparity.bootstraps internal works", {
    data(BeckLee_mat50)
    data <- boot.matrix(BeckLee_mat50, bootstraps = 11, rarefaction = c(5,6))
    metrics_list <- list("level3.fun" = var, "level2.fun" = NULL, "level1.fun" = NULL)
    matrix_decomposition = TRUE
    ## With matrix decomposition
    test0 <- disparity.bootstraps(data$subsets[[1]][[2]], metrics_list, data, matrix_decomposition)
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
        ,as.numeric(c(length(data$call$dimensions), length(data$call$dimensions), data$call$bootstrap[[1]])))

    metrics_list <- list("level3.fun" = NULL, "level2.fun" = variances, "level1.fun" = NULL)
    matrix_decomposition = TRUE
    ## With matrix decomposition
    test1 <- disparity.bootstraps(data$subsets[[1]][[2]], metrics_list, data, matrix_decomposition)

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
        ,as.numeric(c(length(data$call$dimensions), data$call$bootstrap[[1]])))

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
data <- BeckLee_mat50
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
        , 1:26
    )

    error <- capture_error(dim <- dispRity(data, c(sum, ranges), dimensions = 100)$call$dimensions)
    expect_equal(error[[1]] , "Number of dimensions cannot be more than the number of columns in the matrix.")

    # Cannot stack metric level 1 on level 2:
    data(disparity)
    error <- capture_error(dispRity(disparity, metric = variances))
    expect_equal(as.character(error), "Error: At least one metric dimension level 1 was already calculated for disparity.\nImpossible to apply a metric higher than dimension level 1.\n")
    error <- capture_error(dispRity(disparity, metric = c(sum, variances)))
    expect_equal(as.character(error), "Error: At least one metric dimension level 1 was already calculated for disparity.\nImpossible to apply a metric higher than dimension level 1.\n")

    ## Only dimensions 3!
    error <- capture_error(dispRity(data, metric = var))
    expect_equal(error[[1]], "At least one metric must be dimension-level 1 or dimension-level 2\n.For more information, see:\n?make.metric()")
})

#Reset
test <- NULL
data <- BeckLee_mat50

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
test <- dispRity(data, metric = c(sum, ranges))

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
test <- NULL
data <- BeckLee_mat50

#bootstrapped
data <- boot.matrix(data, bootstrap = 5, rarefaction = FALSE, boot.type = "full")
test <- dispRity(data, metric = c(sum, ranges))
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
test <- NULL
data <- BeckLee_mat50

#bootstrapped + rarefied
data <- BeckLee_mat50
data <- boot.matrix(data, bootstrap = 5, rarefaction = TRUE, boot.type = "full")
test <- dispRity(data, metric = c(sum, ranges))
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
test <- NULL
data <- BeckLee_mat50

#one matrix with subsets
data <- custom.subsets(data, group)
test <- dispRity(data, metric = c(sum, ranges))
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
test <- NULL
data <- BeckLee_mat50

#bootstrapped + rarefied + subsets
group <- as.data.frame(matrix(data = c(rep(1, nrow(data)/2),rep(2, nrow(data)/2)), nrow = nrow(data), ncol = 1))
rownames(group) <- rownames(data)
data <- custom.subsets(data, group)
data <- boot.matrix(data, bootstrap = 5, rarefaction = FALSE, boot.type = "full")
test <- dispRity(data, metric = c(sum, ranges))
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
test <- NULL
data <- BeckLee_mat50

#testing example
test_that("Example works", {
    data(BeckLee_mat50)
    sum_of_ranges <- dispRity(BeckLee_mat50, metric = c(sum, ranges))
    ex1 <- summary(sum_of_ranges)
    expect_is(
        ex1, "data.frame"
        )
    expect_equal(
        dim(ex1), c(1,3)
        )

    bootstrapped_data <- boot.matrix(BeckLee_mat50, bootstraps = 100)
    ex2 <- dispRity(bootstrapped_data, metric = c(sum, ranges))
    expect_is(
        ex2, "dispRity"
        )
    expect_equal(
        dim(ex2[[1]][[1]]), c(50,48)
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

    all_ranges <- dispRity(BeckLee_mat50, metric = ranges)
    sum_of_ranges <- dispRity(all_ranges, metric = sum)
    ex1<-summary(sum_of_ranges)
    expect_is(
        ex1, "data.frame"
        )
    expect_equal(
        dim(ex1), c(1,3)
        )
})

#Reset
test <- NULL
data <- BeckLee_mat50

## dispRity works with empty or small (<3 subsets)
test_that("dispRity works with small, empty/subsets", {

    tree <- BeckLee_tree
    data <- BeckLee_mat99
    FADLAD <- BeckLee_ages

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

    expect_equal(as.vector(summary(test1)$obs), c(0.000, 0.002))
    expect_equal(as.vector(summary(test2)$obs), c(-0.004, 0.000))
    expect_equal(as.vector(summary(test3)$obs), c(-0.005, 0.002))
})

test_that("dispRity works with function recycling", {

    set.seed(1)
    mat <- matrix(rnorm(25), 5, 5, dimnames = list(c(1:5)))
    level2 <- dispRity(mat, metric = centroids)
    expect_equal(get.disparity(level2)[[1]], centroids(mat))
    expect_equal(names(level2$call$disparity$metric), c("name", "fun", "between.groups"))
    expect_equal(as.character(level2$call$disparity$metric$name[[1]]), "centroids")

    level1 <- dispRity(level2, metric = mean)
    expect_equal(get.disparity(level1)[[1]], mean(centroids(mat)))
    expect_equal(names(level1$call$disparity$metric), c("name", "fun", "between.groups"))
    expect_equal(as.character(level1$call$disparity$metric$name), c("centroids", "mean"))

    ## With arguments
    level2 <- dispRity(mat, metric = centroids, centroid = 0)
    expect_equal(get.disparity(level2)[[1]], centroids(mat, centroid = 0))
    expect_equal(names(level2$call$disparity$metric), c("name", "fun", "between.groups", "args"))
    expect_equal(as.character(level2$call$disparity$metric$name[[1]]), "centroids")
    expect_equal(level2$call$disparity$metric$args, list("centroid" = 0))

    level1 <- dispRity(level2, metric = mean)
    expect_equal(get.disparity(level1)[[1]], mean(centroids(mat, centroid = 0)))
    expect_equal(names(level1$call$disparity$metric), c("name", "fun", "between.groups", "args"))
    expect_equal(as.character(level1$call$disparity$metric$name), c("centroids", "mean"))
    expect_equal(level2$call$disparity$metric$args, list("centroid" = 0))
})

test_that("dispRity works with multiple trees from time-slicing", {
    load("paleotree_test_data.rda")
    tree <- paleotree_data$tree
    data <- paleotree_data$data

    ## Works with a multiPhylo object
    time_slices_normal <- chrono.subsets(data, tree, method = "continuous", time = 3, model = "proximity")

    ## Test disparity
    test <- dispRity(time_slices_normal, metric = c(sum, variances))
    expect_is(test, "dispRity")
    sum_test <- summary(test)
    expect_equal(sum_test$n, unlist(lapply(test$subsets, lapply, function(x) length(x)/2), use.names = FALSE))
    expect_equal_round(sum_test$obs.median, unlist(lapply(test$disparity, lapply, median), use.names = FALSE), 3)

    ## Works with multiPhylo object and probabilities
    time_slices_proba <- chrono.subsets(data, tree, method = "continuous", time = 3, model = "gradual.split")

    ## Test disparity
    set.seed(1)
    test <- dispRity(time_slices_proba, metric = c(sum, variances))
    expect_is(test, "dispRity")
    sum_test1 <- summary(test)
    expect_equal(sum_test1$n, unlist(lapply(test$subsets, lapply, length), use.names = FALSE) / 6)
    expect_equal_round(sum_test1$obs.median, unlist(lapply(test$disparity, lapply, median), use.names = FALSE), 3)

    ## Works with level 2 metrics only
    set.seed(1)
    test <- dispRity(time_slices_normal, metric = variances)
    expect_is(test, "dispRity")
    sum_test2 <- summary(test)
    expect_equal(sum_test2$n, unlist(lapply(test$subsets, lapply, length), use.names = FALSE) / 2)
    expect_equal_round(sum_test2$obs.median, unlist(lapply(test$disparity, lapply, median), use.names = FALSE), 3)

    set.seed(1)
    test <- dispRity(boot.matrix(time_slices_proba), metric = c(sum, variances))
    expect_is(test, "dispRity")
    sum_test3 <- summary(test)
    expect_equal(sum_test3$n, c(3, 6, 10))
    # expect_equal_round(sum_test3$obs.median[c(1,3)], sum_test1$obs.median[c(1,3)])

    set.seed(1)
    test <- dispRity(boot.matrix(time_slices_normal), metric = variances)
    expect_is(test, "dispRity")
    sum_test4 <- summary(test)
    expect_equal(sum_test4$n, c(3, 5, 10))
    expect_equal(sum_test4$obs.median, sum_test2$obs.median)
})

test_that("get.row.col works", {
    test <- matrix(1, 5, 5)
    expect_equal(dim(get.row.col(test, 1:5)), c(5, 5))
    expect_equal(dim(get.row.col(test, 3:1, 4)), c(3, 4))
})

test_that("dispRity works with multiple matrices", {

    data <- list(matrix(1, 5, 10), matrix(2, 5, 10), matrix(3, 5, 10))

    ## Works with level one
    expect_warning(test <- dispRity(data, metric = c(sum)))
    expect_is(test, "dispRity")
    expect_equal(length(test$disparity[[1]]$elements), 3)
    expect_equal(test$disparity[[1]]$elements, matrix(c(50, 100, 150), nrow = 1))

    means <- function(matrix) apply(matrix, 2, mean)

    ## Works with level two
    expect_warning(test <- dispRity(data, metric = means))
    expect_is(test, "dispRity")
    expect_equal(dim(test$disparity[[1]]$elements), c(10, 3))
    expect_equal(test$disparity[[1]]$elements[1,], c(1,2,3))
    expect_equal(apply(test$disparity[[1]]$elements,2, sd), c(0,0,0))
    expect_equal(summary(test)$obs.median, 2)

    ## Works with piling levels
    test2 <- dispRity(test, metric = sd)
    expect_equal(c(test2$disparity[[1]]$elements), c(0,0,0))

    ## Works with level 2 and subsets
    matrices_list <- space.maker(elements = 30, dimensions = 5, distribution = rnorm, replicates = 3)
    ## warning is added rownames
    expect_warning(matrices_groups <- custom.subsets(data = matrices_list, group = list("group1" = 1:20, "group2" = 21:30)))
    data <- dispRity(data = matrices_groups, metric = centroids)
    expect_equal(dim(data$disparity[[1]]$elements), c(20, 3))
    expect_equal(dim(data$disparity[[2]]$elements), c(10, 3))
})

test_that("dispRity works with multiple matrices from chrono.subsets", {

    load("bound_test_data.rda")
    matrices <- bound_test_data$matrices
    trees <- bound_test_data$trees
    
    ## Test working fine
    test <- chrono.subsets(matrices, tree = trees, time = 3, method = "continuous", model = "gradual.split", t0 = 5)

    expect_is(test, "dispRity")
    expect_is(test$matrix, "list")
    expect_equal(length(test$matrix), 3)
    expect_is(test$matrix[[1]], "matrix")
    expect_equal(rownames(test$matrix[[1]]), rownames(matrices[[1]]))
    expect_is(test$subsets, "list")
    expect_equal(length(test$subsets), 3)
    expect_equal(dim(test$subsets$`5`$elements), c(7, 9))

    ## Calculating disparity works
    level1 <- dispRity(test, metric = mean)
    level2 <- dispRity(test, metric = centroids)
    level12 <- dispRity(test, metric = c(mean, centroids))

    ## level 1 works?
    expect_is(level1, "dispRity")
    ## Results in a 3matrix X 3trees matrix
    expect_equal(length(level1$disparity[[1]][[1]]), 9)
    ## Variance in the two first subsets (nodes are different)
    expect_true(sd(level1$disparity[[1]][[1]]) != 0)
    expect_true(sd(level1$disparity[[2]][[1]]) != 0)
    ## No variance in the third (only tips which are the same in this design)
    # expect_false(sd(level1$disparity[[3]][[1]]) != 0) #bug in macoss
    # expect_equal(summary(level1)$obs.median, c(-0.190, -0.243, -0.164))

    ## level2 works?
    expect_is(level2, "dispRity")
    ## Results is length elements * matrices * trees
    expect_equal(dim(level2$disparity[[1]][[1]]), c(21,3))
    expect_equal(dim(level2$disparity[[2]][[1]]), c(24,3))
    expect_equal(dim(level2$disparity[[3]][[1]]), c(30,3))
    ## Correct results (should be equal to level12?)
    # expect_equal(summary(level2, cent.tend = mean, na.rm = TRUE)$obs.mean, c(0.410, 0.814, 1.217))

    ## level12 works?
    expect_is(level12, "dispRity")
    ## results is length trees?
    expect_equal(length(level12$disparity[[1]][[1]]), 3)
    expect_equal(length(level12$disparity[[2]][[1]]), 3)
    expect_equal(length(level12$disparity[[3]][[1]]), 3)
    ## Variance in the two first subsets (nodes are different)
    expect_true(is.na(sd(level12$disparity[[1]][[1]])))
    expect_true(sd(level1$disparity[[2]][[1]]) != 0)
    ## No variance in the third (only tips which are the same in this design)
    # expect_false(sd(level1$disparity[[3]][[1]]) != 0) #bug in macos
    # expect_equal(summary(level12, cent.tend = mean, na.rm = TRUE)$obs.mean, c(0.580, 0.654, 1.217))

    ## Works with binding data
    set.seed(1)
    test <- chrono.subsets(matrices, tree = trees, time = 3, method = "continuous", model = "acctran", t0 = 5, bind.data = TRUE)

    means <- dispRity(test, metric = mean, na.rm = TRUE)
    expect_is(means, "dispRity")
    expect_equal(as.vector(means$disparity[[1]]$elements),
            c(mean(means$matrix[[1]][means$subsets[[1]]$elements[,1],]),
              mean(means$matrix[[2]][means$subsets[[1]]$elements[,2],]),
              mean(means$matrix[[3]][means$subsets[[1]]$elements[,3],], na.rm = TRUE))
    )
    expect_equal(as.vector(means$disparity[[2]]$elements),
            c(mean(means$matrix[[1]][means$subsets[[2]]$elements[,1],]),
              mean(means$matrix[[2]][means$subsets[[2]]$elements[,2],]),
              mean(means$matrix[[3]][means$subsets[[2]]$elements[,3],]))
    )
    expect_equal(as.vector(means$disparity[[3]]$elements),
            c(mean(means$matrix[[1]][means$subsets[[3]]$elements[,1],]),
              mean(means$matrix[[2]][means$subsets[[3]]$elements[,2],]),
              mean(means$matrix[[3]][means$subsets[[3]]$elements[,3],]))
    )

    ## Works with unpaired number of trees and matrices
    test <- chrono.subsets(matrices[[1]], tree = trees, time = 3, method = "continuous", model = "acctran", t0 = 5, bind.data = FALSE)
    means <- dispRity(test, metric = mean, na.rm = TRUE)
    expect_is(means, "dispRity")
    test <- chrono.subsets(matrices, tree = trees[[1]], time = 3, method = "continuous", model = "acctran", t0 = 5, bind.data = FALSE)
    means <- dispRity(test, metric = mean, na.rm = TRUE)
    expect_is(means, "dispRity")

    ## Works with recycling dispRity objects
    vars <- dispRity(test, metric = variances)
    sumvars <- dispRity(test, metric = c(sum, variances))
    sumvars2 <- dispRity(vars, metric = sum)    

    expect_is(vars, "dispRity")
    expect_is(sumvars, "dispRity")
    expect_is(sumvars2, "dispRity")
    expect_equal(
        as.vector(sumvars$disparity[[1]]$elements),
        as.vector(sumvars2$disparity[[1]]$elements))
    expect_equal(
        as.vector(sumvars$disparity[[2]]$elements),
        as.vector(sumvars2$disparity[[2]]$elements))
    expect_equal(
        as.vector(sumvars$disparity[[2]]$elements),
        as.vector(sumvars2$disparity[[2]]$elements))


    ## Works with bootstraps
    test1 <- boot.matrix(test, bootstraps = 12)

    vars <- dispRity(test1, metric = variances)
    sumvars <- dispRity(test1, metric = c(sum, variances))
    sumvars2 <- dispRity(vars, metric = sum)

    expect_equal(
        as.vector(sumvars$disparity[[1]]$elements),
        as.vector(sumvars2$disparity[[1]]$elements))
    expect_equal(
        as.vector(sumvars$disparity[[2]]$elements),
        as.vector(sumvars2$disparity[[2]]$elements))
    expect_equal(
        as.vector(sumvars$disparity[[2]]$elements),
        as.vector(sumvars2$disparity[[2]]$elements))

    ## Predictable values
    matrices[[1]][,] <- 1
    matrices[[2]][,] <- 4
    matrices[[3]][,] <- 10

    ## Bound and unbound data
    unbou <- chrono.subsets(matrices, tree = trees, time = 3, method = "continuous", model = "equal.split", t0 = 5, bind.data = FALSE)
    bound <- chrono.subsets(matrices, tree = trees, time = 3, method = "continuous", model = "equal.split", t0 = 5, bind.data = TRUE)

    ## Getting the mean
    bs_unbou <- dispRity(boot.matrix(unbou, 102), metric = mean)
    bs_bound <- dispRity(boot.matrix(bound, 102), metric = mean)

    expect_equal(
        summary(bs_bound, cent.tend = mean)$bs.mean,
        c(5,5,5))
    expect_equal(
        summary(bs_unbou, cent.tend = mean)$bs.mean,
        c(5,5,5))

    expect_equal(dim(bs_unbou$disparity[[1]]$elements), c(3,3))
    expect_equal(dim(bs_bound$disparity[[1]]$elements), c(1,3))
})

test_that("dispRity works with the tree component", {

    ## A ladderized tree
    set.seed(1)
    tree <- read.tree(text = "((((((A,B), C), D), E), F), G);")
    tree$edge.length <- rep(1, 7+6)
    tree$node.label <- letters[1:6]
    ## An empty matrix (with the right elements)
    matrix <- matrix(rnorm((7+6)*2), nrow = 7+6, ncol = 2)
    rownames(matrix) <- c(tree$tip.label, tree$node.label)

    ## Simple test
    test <- dispRity(data = matrix, metric = edge.length.tree, tree = tree)
    expect_equal(c(test$disparity[[1]][[1]]), edge.length.tree(matrix, tree))

    ## More complex metric test
    test <- dispRity(data = matrix, metric = projections.tree, tree = tree, type = c("root","ancestor"))
    expect_equal(c(test$disparity[[1]][[1]]), projections.tree(matrix, tree))

    ## Simple test with custom subsets
    data <- custom.subsets(matrix, group = list(LETTERS[1:5], letters[1:5]))
    test <- dispRity(data = data, metric = edge.length.tree, tree = tree)
    expect_equal(c(test$disparity[[1]][[1]]), edge.length.tree(matrix[LETTERS[1:5], ], tree))
    expect_equal(c(test$disparity[[2]][[1]]), edge.length.tree(matrix[letters[1:5], ],tree))

    ## Simple test with custom subsets and bootstraps
    data <- custom.subsets(matrix, group = list(LETTERS[1:5], letters[1:5]))
    test <- dispRity(data = boot.matrix(data, rarefaction = 4), metric = edge.length.tree, tree = tree)
    expect_null(plot(test))

    ## Test from previous metric
    ntips <- function(matrix, tree) {
        Ntip(tree)
    }
    data <- dispRity(data = matrix, metric = centroids)
    expect_equal(sort(data$disparity[[1]][[1]]), unname(sort(centroids(matrix))))
    test <- dispRity(data = data, metric = ntips, tree = tree)
    expect_equal(test$disparity[[1]][[1]][[1]], Ntip(tree))

    ## From inherited tree
    data <- custom.subsets(matrix, group = list(LETTERS[1:5], letters[1:5]), tree = tree)
    test <- dispRity(data = data, metric = edge.length.tree)
    expect_equal(c(test$disparity[[1]][[1]]), edge.length.tree(matrix[LETTERS[1:5], ], tree))
    expect_equal(c(test$disparity[[2]][[1]]), edge.length.tree(matrix[letters[1:5], ], tree))

    ## More complex metric test
    test <- dispRity(data = data, metric = projections.tree)
    expect_equal(c(test$disparity[[1]][[1]]), projections.tree(matrix[LETTERS[1:5], ], tree, reference.data = matrix))
    expect_equal(c(test$disparity[[2]][[1]]), projections.tree(matrix[letters[1:5], ], tree, reference.data = matrix))

    ## Works with multiple matrices and single tree
    set.seed(1)
    mulmatrix <- list(matrix, matrix, matrix)
    mulmatrix[[1]][] <- rnorm((7+6)*2)
    mulmatrix[[2]][] <- rnorm((7+6)*2)
    mulmatrix[[3]][] <- rnorm((7+6)*2)
    test <- dispRity(data = mulmatrix, metric = projections.tree, tree = tree)
    expect_equal(c(test$disparity[[1]][[1]][,1]), projections.tree(mulmatrix[[1]], tree))
    expect_equal(c(test$disparity[[1]][[1]][,2]), projections.tree(mulmatrix[[2]], tree))
    expect_equal(c(test$disparity[[1]][[1]][,3]), projections.tree(mulmatrix[[3]], tree))

    ## Works with multiple trees and single matrix
    set.seed(1)
    tree2 <- rtree(dim(matrix)[1]/2+1)
    tree3 <- rtree(dim(matrix)[1]/2+1)
    tree2$tip.label <- tree3$tip.label <- LETTERS[1:7]
    tree2$node.label <- tree3$node.label <- letters[1:6]

    multree <- c(tree, tree2, tree3)
    test <- dispRity(data = matrix, metric = projections.tree, tree = multree)
    expect_equal(c(test$disparity[[1]][[1]][,1]), projections.tree(matrix, tree))
    expect_equal(c(test$disparity[[1]][[1]][,2]), projections.tree(matrix, tree2))    
    expect_equal(c(test$disparity[[1]][[1]][,3]), projections.tree(matrix, tree3))

    ## Works with multiple matrices and trees
    test <- dispRity(data = mulmatrix, metric = projections.tree, tree = multree)
    expect_equal(c(test$disparity[[1]][[1]][,1]), projections.tree(mulmatrix[[1]], tree))
    expect_equal(c(test$disparity[[1]][[1]][,2]), projections.tree(mulmatrix[[2]], tree2))    
    expect_equal(c(test$disparity[[1]][[1]][,3]), projections.tree(mulmatrix[[3]], tree3))


    ## Test with between groups and tree
    between.groups.edge.length.tree <- function(matrix, matrix2, tree) {
        return(edge.length.tree(matrix, tree) - edge.length.tree(matrix2, tree))
    }
    data <- custom.subsets(matrix, group = list(LETTERS[1:5], letters[1:5]), tree = tree)
    ## Working with matrix decomposition
    test <- dispRity(data = data, metric = between.groups.edge.length.tree, tree = tree, between.groups = TRUE)
    expect_equal(unlist(c(unname(summary(test)))), c("1:2", "5", "5", "3", "-0.8", "1", "5", "5.9"))
})

test_that("dispRity works with dist.data", {

    set.seed(1)
    data <- matrix(rnorm(50), 10, 5, dimnames = list(letters[1:10]))
    dist <- as.matrix(dist(matrix(rnorm(45), 9, 5, dimnames = list(letters[1:9]))))
    
    ## Basics (options parsed)
    test_dist <- dispRity(data = dist, metric = centroids, dist.data = TRUE)
    test_data <- dispRity(data = dist, metric = centroids, dist.data = FALSE)
    expect_equal(summary(test_dist)$obs.median, 2.839)
    expect_equal(summary(test_data)$obs.median, 2.839)
    
    ## Subsets (different results)
    warn <- capture_warning(cust <- custom.subsets(dist, group = list(c(1:4), c(5:9))))
    expect_equal(warn[[1]], "custom.subsets is applied on what seems to be a distance matrix.\nThe resulting matrices won't be distance matrices anymore!\nYou can use dist.data = TRUE, if you want to keep the data as a distance matrix.")
    expect_warning(cust <- custom.subsets(dist, group = list(c(1:4), c(5:9))))
    test_dist <- dispRity(data = cust, metric = centroids, dist.data = TRUE)
    test_data <- dispRity(data = cust, metric = centroids, dist.data = FALSE)
    expect_equal(summary(test_dist)$obs.median, c(2.163, 2.925))
    expect_equal(summary(test_data)$obs.median, c(2.679, 3.268))

    ## Subsets (with dist.data recycled)
    cust <- custom.subsets(dist, group = list(c(1:4), c(5:9)), dist.data = TRUE)
    test_dist <- dispRity(data = cust, metric = centroids)
    test_data <- dispRity(data = cust, metric = centroids, dist.data = TRUE)
    expect_equal(summary(test_dist)$obs, c(2.163, 2.925))
    expect_equal(summary(test_data)$obs, c(2.163, 2.925))
    ## Force toggle off
    warn <- capture_warning(test_data <- dispRity(data = cust, metric = centroids, dist.data = FALSE))
    expect_equal(warn[[1]], "data.dist is set to FALSE (the data will not be treated as a distance matrix) even though cust contains distance treated data.")
    expect_warning(test_data <- dispRity(data = cust, metric = centroids, dist.data = FALSE))
    expect_equal(summary(test_data)$obs.median, c(2.679, 3.268))

    ## Bootstraps
    set.seed(1)
    boot <- boot.matrix(dist, boot.by = "dist")
    expect_equal(boot$call$bootstrap[[4]], "dist")
    test_dist <- dispRity(data = boot, metric = centroids, dist.data = TRUE)
    warn <- capture_warning(test_data <- dispRity(data = boot, metric = centroids, dist.data = FALSE))
    expect_equal(warn[[1]], "data.dist is set to FALSE (the data will not be treated as a distance matrix) even though boot contains distance treated data.")
    expect_warning(test_data <- dispRity(data = boot, metric = centroids, dist.data = FALSE))
    expect_equal(summary(test_dist)$bs.median, 3.545)
    expect_equal(summary(test_data)$bs.median, 2.874)
    ## Inherits properly
    test_dist <- dispRity(data = boot, metric = centroids)
    test_data <- dispRity(data = boot, metric = centroids)
    expect_equal(summary(test_dist)$bs.median, 3.545)
    expect_equal(summary(test_data)$bs.median, 3.545)

    ## Bootstraps + subsets
    cust <- custom.subsets(dist, group = list(c(1:4), c(5:9)), dist.data = TRUE)
    ## Warning because no distance!
    warn <- capture_warning(boot.matrix(cust))
    expect_equal(warn[[1]], "boot.by not set to \"dist\" (the data will not be treated as a distance matrix) even though cust contains distance treated data.")
    set.seed(1)
    expect_warning(boot_data <- boot.matrix(cust, boot.by = "rows"))
    set.seed(1)
    boot_dist <- boot.matrix(cust, boot.by = "dist")
    expect_equal(summary(dispRity(data = boot_dist, metric = centroids))$obs.median, c(2.163, 2.925))
    expect_equal(summary(dispRity(data = boot_data, metric = centroids))$obs.median, c(2.679, 3.268))

    expect_warning(cust <- custom.subsets(dist, group = list(c(1:4), c(5:9))))
    set.seed(1)
    boot_data <- boot.matrix(cust)
    set.seed(1)
    boot_dist <- boot.matrix(cust, boot.by = "dist")
    expect_equal(summary(dispRity(data = boot_dist, metric = centroids))$bs.median, c(2.103, 3.140))
    expect_equal(summary(dispRity(data = boot_data, metric = centroids))$bs.median, c(2.173, 2.768))
})

test_that("dispRity works with boot.by = columns", {

    ## Toggle do_by.col in dispRity
    ## Then pass it to lapply wrapper to change the variable by.col from NULL do subset$elements.

    set.seed(1)
    data <- matrix(rnorm(50), 10, 5, dimnames = list(letters[1:10]))
    cust <- custom.subsets(data, group = list(c(1:4), c(5:10)))

    set.seed(1)
    boot_test1 <- boot.matrix(cust, bootstraps = 3, boot.by = "rows")
    expect_equal(boot_test1$subsets[[1]]$elements, matrix(1:4, 4, 1))
    expect_equal(dim(boot_test1$subsets[[1]][[2]]), c(4, 3))
    expect_equal(boot_test1$subsets[[2]]$elements, matrix(5:10, 6, 1))
    expect_equal(dim(boot_test1$subsets[[2]][[2]]), c(6, 3))
    expect_equal(boot_test1$call$bootstrap[[4]], "rows")
    expect_equal(summary(dispRity(boot_test1, metric = centroids))$bs.median, c(1.086, 1.156))

    set.seed(1)
    boot_test2 <- boot.matrix(cust, bootstraps = 3, boot.by = "columns")
    expect_equal(boot_test2$subsets[[1]]$elements, matrix(1:4, 4, 1))
    expect_equal(dim(boot_test2$subsets[[1]][[2]]), c(5, 3))
    expect_equal(boot_test2$subsets[[2]]$elements, matrix(5:10, 6, 1))
    expect_equal(dim(boot_test2$subsets[[2]][[2]]), c(5, 3))
    expect_equal(boot_test2$call$bootstrap[[4]], "columns")
    expect_equal(summary(dispRity(boot_test2, metric = centroids))$bs.median, c(1.919, 1.337))
})


# test_that("dispRity compact works", {

#     compact.matrix <- function(matrix_list) {
        
#         ## Finding the rows in common
#         common_names <- lapply(matrix_list, rownames)
#         while(length(common_names) > 2) {
#             common_names[[1]] <- intersect(common_names[[1]], common_names[[2]])
#             common_names[[2]] <- NULL
#         }
#         common_names <- common_names[[1]]

#         ## Finding the equal rows
#         get.equal.rows <- function(other_matrices, first_matrix, common_names) {
#             apply(other_matrices[common_names, ] == first_matrix[common_names, ], 1, all)
#         }
#         common_values <- lapply(matrix_list[-1], get.equal.rows, matrix_list[[1]], common_names)
#         while(length(common_values) > 2) {
#             common_values[[1]] <- common_values[[1]] & common_values[[2]]
#             common_values[[2]] <- NULL
#         }
#         common_values <- common_values[[1]]

#         ## Separating the matrices
#         remove.common <- function(matrix, common_values) {
#             return(matrix[!(rownames(matrix) %in% c(names(which(common_values)))), ])
#         }
#         common_matrix <- matrix_list[[1]][names(which(common_values)), ]
#         other_matrices <- lapply(matrix_list, remove.common, common_values)
#         return(unlist(list(list("common" = common_matrix), other_matrices), recursive = FALSE))
#     }

#     expand.matrix <- function(matrix_list) {
#         ## Simply return the matrix
#         if(names(matrix_list)[[1]] != "common") {
#             return(matrix_list)
#         } else {
#             return(lapply(matrix_list[-1], rbind, matrix_list$common))
#         }
#     }

    ##TODO: implementation: search/replace ...$matrix -> expand.matrix(...$matrix)


#     mat1 <- matrix(rnorm(20), 10, 2)
#     mat2 <- matrix(rnorm(20), 10, 2)
#     mat3 <- matrix(rnorm(24), 12, 2)
#     rownames(mat1) <- rownames(mat2) <- letters[1:10]
#     rownames(mat2)[10] <- "root"
#     rownames(mat3) <- letters[1:12]
#     ## a b d and e are common within all matrices
#     mat1[c("a", "b", "d", "e"),] -> mat2[c("a", "b", "d", "e"),] -> mat3[c("a", "b", "d", "e"),]

#     matrix_list <- list(mat1, mat2, mat3)

#     test <- compact.matrix(matrix_list)
#     test <- expand.matrix(test)
# })







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

