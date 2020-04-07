# TESTING dispRity

context("dispRity")

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


test_that("chrono.subsets + ancestral.dist doesn't work yet", {

    expect_warning(error <- capture_error(dispRity(data_subsets_simple, metric = ancestral.dist, tree = BeckLee_tree)))
    expect_equal(error[[1]], "ancestral.dist cannot be calculated on dispRity objects with chrono.subsets yet.\nThis will be available in the next dispRity version.\nYou can contact me (guillert@tcd.ie) for more info.")
})


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
        ,as.numeric(c(data$call$dimensions, data$call$dimensions, data$call$bootstrap[[1]])))

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
        , 26
    )

    warn <- capture_warnings(dim <- dispRity(data, c(sum, ranges), dimensions = 100)$call$dimensions)
    expect_equal(dim, 48)
    expect_equal(warn , "Dimension number too high: set to 48.")

    # Cannot stack metric level 1 on level 2:
    data(disparity)
    error <- capture_error(dispRity(disparity, metric = variances))
    expect_equal(as.character(error), "Error: At least one metric dimension level 1 was already calculated for disparity.\nImpossible to apply a metric higher than dimension level 1.\n")
    error <- capture_error(dispRity(disparity, metric = c(sum, variances)))
    expect_equal(as.character(error), "Error: At least one metric dimension level 1 was already calculated for disparity.\nImpossible to apply a metric higher than dimension level 1.\n")

    ## Only dimensions 3!
    error <- capture_error(dispRity(data, metric = var))
    expect_equal(error[[1]], "var metric must contain at least a dimension-level 1 or a dimension-level 2 metric.\nFor more information, see ?make.metric.")
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

    expect_equal(as.vector(summary(test1)$obs), c(0.009, 0.007))
    expect_equal(as.vector(summary(test2)$obs), c(-0.002, 0.011))
    expect_equal(as.vector(summary(test3)$obs), c(0.003, 0.007))
})


test_that("dispRity works with function recycling", {

    set.seed(1)
    mat <- matrix(rnorm(25), 5, 5, dimnames = list(c(1:5)))
    level2 <- dispRity(mat, metric = centroids)
    expect_equal(extract.dispRity(level2)[[1]], centroids(mat))
    expect_equal(names(level2$call$disparity$metric), c("name", "fun"))
    expect_equal(as.character(level2$call$disparity$metric$name[[1]]), "centroids")

    level1 <- dispRity(level2, metric = mean)
    expect_equal(extract.dispRity(level1)[[1]], mean(centroids(mat)))
    expect_equal(names(level1$call$disparity$metric), c("name", "fun"))
    expect_equal(as.character(level1$call$disparity$metric$name), c("centroids", "mean"))

    ## With arguments
    level2 <- dispRity(mat, metric = centroids, centroid = 0)
    expect_equal(extract.dispRity(level2)[[1]], centroids(mat, centroid = 0))
    expect_equal(names(level2$call$disparity$metric), c("name", "fun", "args"))
    expect_equal(as.character(level2$call$disparity$metric$name[[1]]), "centroids")
    expect_equal(level2$call$disparity$metric$args, list("centroid" = 0))

    level1 <- dispRity(level2, metric = mean)
    expect_equal(extract.dispRity(level1)[[1]], mean(centroids(mat, centroid = 0)))
    expect_equal(names(level1$call$disparity$metric), c("name", "fun", "args"))
    expect_equal(as.character(level1$call$disparity$metric$name), c("centroids", "mean"))
    expect_equal(level2$call$disparity$metric$args, list("centroid" = 0))
})


test_that("dispRity works with multiple trees from time-slicing", {
    load("paleotree_test_data.Rda")
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
    expect_equal(sum_test3$n, c(3, 7, 10))
    expect_equal_round(sum_test3$obs.median[c(1,3)], sum_test1$obs.median[c(1,3)])

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

    set.seed(42)
    one_matrix <- matrix(1, 5, 10)
    data <- list(one_matrix, one_matrix, one_matrix)

    ## Works with level one
    expect_warning(test <- dispRity(data, metric = c(sum)))
    expect_is(test, "dispRity")
    expect_equal(length(test$disparity[[1]]$elements), 3)
    expect_equal(sum(test$disparity[[1]]$elements), 150)

    ## Works with level two
    expect_warning(test <- dispRity(data, metric =  ranges))
    expect_is(test, "dispRity")
    expect_equal(length(test$disparity[[1]]$elements), 30)
})

test_that("dispRity works with multiple matrices from chrono.subsets", {

    load("bound_test_data.Rda")
    matrices <- bound_test_data$matrices
    trees <- bound_test_data$trees
    
    ## Test working fine
    test <- chrono.subsets(matrices, tree = trees, time = 3, method = "continuous", model = "gradual.split", t0 = 5)

    expect_is(test, "dispRity")
    expect_is(test$matrix, "list")
    expect_equal(length(test$matrix), 3)
    expect_is(test$matrix[[1]], "matrix")
    expect_equal(rownames(test$matrix[[1]]), sort(c(trees[[1]]$tip.label, trees[[1]]$node.label)))
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
    ## Variance in the two first subsets (nodes are different)
    expect_true(sd(level1$disparity[[1]][[1]]) != 0)
    expect_true(sd(level1$disparity[[2]][[1]]) != 0)
    ## No variance in the third (only tips which are the same in this design)
    expect_false(sd(level1$disparity[[3]][[1]]) != 0)
    expect_equal(summary(level1)$obs.median, c(-0.186, -0.187, -0.164))

    ## level2 works?
    expect_is(level2, "dispRity")
    ## Results is length elements * matrices * trees
    expect_equal(dim(level2$disparity[[1]][[1]]), c(21,3))
    expect_equal(dim(level2$disparity[[2]][[1]]), c(24,3))
    expect_equal(dim(level2$disparity[[3]][[1]]), c(30,3))
    ## Correct results (should be equal to level12?)
    expect_equal(summary(level2, cent.tend = mean, na.rm = TRUE)$obs.mean, c(0.467, 0.770, 1.217))

    ## level12 works?
    expect_is(level12, "dispRity")
    ## results is length trees?
    expect_equal(length(level12$disparity[[1]][[1]]), 3)
    expect_equal(length(level12$disparity[[2]][[1]]), 3)
    expect_equal(length(level12$disparity[[3]][[1]]), 3)
    ## Variance in the two first subsets (nodes are different)
    expect_true(is.na(sd(level12$disparity[[1]][[1]])))
    expect_true(sd(level1$disparity[[2]][[1]]) != 0)
    ## No variance in the third (only tips which are the same in this design)
    expect_false(sd(level1$disparity[[3]][[1]]) != 0)
    expect_equal(summary(level12, cent.tend = mean, na.rm = TRUE)$obs.mean, c(0.475, 0.801, 1.217))

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


    ## Predictable values
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
