## Test
#package_coverage(type = "tests", quiet = FALSE, clean = FALSE)
test_that("as.covar works in standalone", {

    # {

    ## Creating a dispRity
    data(charadriiformes)
    covar_data <- MCMCglmm.subsets(data       = charadriiformes$data,
                                   posteriors = charadriiformes$posteriors)    

    ## Testing the handling
    match_call <- list()

    ## level 3 covar
    var.mat <- function(matrix, ...) {var(matrix, ...)}
    metric <- as.covar(var.mat)

    expect_true(check.covar(metric, covar_data)$is_covar)
    test <- get.dispRity.metric.handle(c(sum, metric), match_call, data = covar_data, tree = NULL)$levels 
    expect_true(!is.null(test$level3.fun))
    expect_true(is.null(test$level2.fun))
    expect_true(!is.null(test$level1.fun))
    expect_true(eval.covar(test$level3.fun, null.return = FALSE))
    expect_false(eval.covar(test$level1.fun, null.return = FALSE))

    ## level 2 covar
    metric <- as.covar(variances)
    expect_true(check.covar(metric, covar_data)$is_covar)
    test <- get.dispRity.metric.handle(metric, match_call, data = covar_data, tree = NULL)$levels
    expect_true(is.null(test$level3.fun))
    expect_true(!is.null(test$level2.fun))
    expect_true(is.null(test$level1.fun))
    expect_true(eval.covar(test$level2.fun, null.return = FALSE))

    ## level 1 covar (with no formals)
    # sum.mat <- function(matrix, ...) {var(matrix, ...)}
    metric <- as.covar(sum)
    expect_true(check.covar(metric, covar_data)$is_covar)
    test <- get.dispRity.metric.handle(metric, match_call, data = covar_data, tree = NULL)$levels
    expect_true(is.null(test$level3.fun))
    expect_true(is.null(test$level2.fun))
    expect_true(!is.null(test$level1.fun))
    expect_true(eval.covar(test$level1.fun, null.return = FALSE))

    ## level 1 covar (with formals)
    metric <- as.covar(ellipsoid.volume)
    expect_true(check.covar(metric, covar_data)$is_covar)
    test <- get.dispRity.metric.handle(metric, match_call, data = covar_data, tree = NULL)$levels
    expect_true(is.null(test$level3.fun))
    expect_true(is.null(test$level2.fun))
    expect_true(!is.null(test$level1.fun))
    expect_true(eval.covar(test$level1.fun, null.return = FALSE))

    ## pairs of metrics:
    # Possible combinations:
    # covar(lvl1)
    # lvl1 + covar(lvl2)
    # lvl1 + lvl2 + covar(lvl3)

    metric <- c(sum, as.covar(variances))
    test <- get.dispRity.metric.handle(metric, match_call, data = covar_data, tree = NULL)$levels
    expect_true(is.null(test$level3.fun))
    expect_true(!is.null(test$level2.fun))
    expect_true(!is.null(test$level1.fun))
    expect_true(eval.covar(test$level2.fun, null.return = FALSE))
    expect_false(eval.covar(test$level1.fun, null.return = FALSE))

    test <- as.covar(var)
    expect_equal(names(formals(test))[[1]], "x")
    expect_equal(deparse(body(test))[[3]], "    return(fun(x = x$VCV, ...))")
    expect_true(eval.covar(test))


    metric <- c(sd, variances, as.covar(var))
    test <- get.dispRity.metric.handle(metric, match_call, data = covar_data, tree = NULL)$levels
    expect_true(!is.null(test$level3.fun))
    expect_true(!is.null(test$level2.fun))
    expect_true(!is.null(test$level1.fun))
    expect_true(eval.covar(test$level3.fun, null.return = FALSE))
    expect_false(eval.covar(test$level2.fun, null.return = FALSE))
    expect_false(eval.covar(test$level1.fun, null.return = FALSE))

    metric <- c(as.covar(sum), variances)
    error <- capture_error(get.dispRity.metric.handle(metric, match_call, data = covar_data, tree = NULL))
    expect_equal(error[[1]], "Only the highest dimension-level metric can be set as as.covar().")
    metric <- c(as.covar(sum), as.covar(variances))
    error <- capture_error(get.dispRity.metric.handle(metric, match_call, data = covar_data, tree = NULL))
    expect_equal(error[[1]], "Only one metric can be set as as.covar().")
})

test_that("as.covar works in dispRity", {

    # {

    data(charadriiformes)

    ## Test works OK with base
    data0 <- custom.subsets(data  = charadriiformes$data[, -c(18, 19)],
                            group = charadriiformes$data[, "clade"])
    test0 <- dispRity(data0, metric = c(sum, variances))
    expect_is(test0, "dispRity")
    expect_equal(names(test0), c("matrix", "tree", "call", "subsets", "disparity"))
    expect_equal(c(summary(test0)$obs), c(0.272, 0.351, 0.360))

    ## Same but selecting only the three first random terms
    data <- MCMCglmm.subsets(data       = charadriiformes$data,
                             posteriors = charadriiformes$posteriors,
                             tree       = charadriiformes$tree,
                             group      = MCMCglmm.levels(
                                     charadriiformes$posteriors)[1:3],
                             rename.groups = c("gulls", "plovers", "sandpipers"))

    ## The subsets are the same
    for(i in 1:3) {
        expect_equal(c(data0$subsets[[i]]$elements), c(data$subsets[[i]]$elements))
    }

    ## Test works OK with base + covar (same as test0)
    test1 <- dispRity(data, metric = c(sum, variances), dimensions = c(1:17))
    expect_is(test1, "dispRity")
    expect_equal(names(test1), c("matrix", "tree", "call", "subsets", "covar", "disparity"))
    expect_equal(c(summary(test1)$obs), c(0.272, 0.351, 0.360))

    ## Test works in 2 times
    testA <- dispRity(data, metric = variances, dimensions = c(1:17))
    expect_is(testA, "dispRity")
    expect_equal(names(testA), c("matrix", "tree", "call", "subsets", "covar", "disparity"))
    testB <- dispRity(testA, metric = sum)
    expect_is(testB, "dispRity")
    expect_equal(names(testB), c("matrix", "tree", "call", "subsets", "covar", "disparity"))
    expect_equal(c(summary(testB)$obs), c(0.272, 0.351, 0.360))

    ## Test works OK with as.covar
    test2 <- dispRity(data, metric = c(sum, as.covar(variances)))
    expect_is(test2, "dispRity")
    expect_equal(names(test2), c("matrix", "tree", "call", "subsets", "covar", "disparity"))
    ## Different results
    expect_equal(c(summary(test2)$obs), c(0.026, 0.000, 0.002))

    ## Test works in 2 times (1st covar)
    testA <- dispRity(data, metric = as.covar(variances), dimensions = c(1:17))
    expect_is(testA, "dispRity")
    expect_equal(names(testA), c("matrix", "tree", "call", "subsets", "covar", "disparity"))
    expect_equal(c(summary(testA)$`97.5%`), c(0.068, 0.002, 0.016))
    ## Works with level 1
    testB <- dispRity(testA, metric = sum)
    expect_is(testB, "dispRity")
    expect_equal(names(testB), c("matrix", "tree", "call", "subsets", "covar", "disparity"))
    expect_equal(c(summary(testB)$obs), c(0.026, 0.000, 0.002))
    ## Error if level 1 is also covar
    error <- capture_error(dispRity(testA, metric = as.covar(sum)))
    expect_equal(error[[1]], "Impossible to apply a metric as.covar() on a dispRity object that already contains disparity results.")
    ## But works with just a level 1
    test <- dispRity(data, metric = as.covar(sum))
    expect_equal(summary(test)$obs.median, c(0.213, 0.016, 0.088))

    ## Test works with extra arguments
    test1 <- dispRity(data, metric = c(sum, as.covar(centroids)))
    test2 <- dispRity(data, metric = c(sum, as.covar(centroids)), centre = 100)
    expect_is(test1, "dispRity")
    expect_is(test2, "dispRity")
    expect_equal(names(test1), c("matrix", "tree", "call", "subsets", "covar", "disparity"))
    expect_equal(names(test2), c("matrix", "tree", "call", "subsets", "covar", "disparity"))
    ## Different results
    expect_equal(c(summary(test1)$obs), c(0.375, 0.017, 0.112))
    expect_equal(c(summary(test2)$obs), c(100.4, 100.0, 100.1))

    ## Test with VCV, loc toggles
    sum.var.dist <- function(matrix, loc = rep(0, ncol(matrix))) {
        if(!is(matrix, "matrix")) {
            matrix <- diag(matrix)
        }
        ## Get the sum of the diagonal of the matrix
        sum_diag <- sum(diag(matrix))
        ## Get the distance between 0 and the loc
        dist_loc <- dist(matrix(c(rep(0, ncol(matrix)), loc), nrow = 2, byrow = TRUE))[1]
        ## Return the sum of the diagonal minus the distance
        return(sum_diag - dist_loc)
    }
    sum.var.group <- function(matrix, matrix2, loc, loc2, ...) {
        if(missing(loc)) {
            if(!is(matrix, "matrix")) {
                loc = rep(0, length(matrix))
            } else {
                loc = rep(0, ncol(matrix))
            }       
        }
        if(missing(loc2)) {
            if(!is(matrix2, "matrix")) {
                loc2 = rep(0, length(matrix2))
            } else {
                loc2 = rep(0, ncol(matrix2))
            }       
        }
        return(sum.var.dist(matrix, loc) + sum.var.dist(matrix2, loc2))
    }


    ## Test works OK with base
    set.seed(42)
    data <- MCMCglmm.subsets(data       = charadriiformes$data,
                             posteriors = charadriiformes$posteriors,
                             tree       = charadriiformes$tree,
                             group      = MCMCglmm.levels(
                                     charadriiformes$posteriors)[1:3],
                             rename.groups = c("gulls", "plovers", "sandpipers"),
                             n = 3)
    ## Adding some loc for groups 2 and 3
    data$covar[[2]][[1]]$loc <- data$covar[[2]][[2]]$loc <- data$covar[[2]][[3]]$loc <- rep(1, 3)
    data$covar[[3]][[1]]$loc <- data$covar[[3]][[2]]$loc <- data$covar[[3]][[3]]$loc <- rep(10, 3)

    ## VCV && !loc
    test2 <- dispRity(data, metric = as.covar(sum.var.dist, VCV = TRUE, loc = FALSE))
    expect_is(test2, "dispRity")
    expect_equal(names(test2), c("matrix", "tree", "call", "subsets", "covar", "disparity"))
    ## Different results
    expect_equal(c(summary(test2)$obs), c(0.384, 0.046, 0.147))

    ## !VCV && loc
    test2 <- dispRity(data, metric = as.covar(sum.var.dist, VCV = FALSE, loc = TRUE))
    expect_is(test2, "dispRity")
    expect_equal(names(test2), c("matrix", "tree", "call", "subsets", "covar", "disparity"))
    ## Different results
    expect_equal(c(summary(test2)$obs), c(0, 1, 10))

    ## VCV && loc
    test2 <- dispRity(data, metric = as.covar(sum.var.dist, VCV = TRUE, loc = TRUE))
    expect_is(test2, "dispRity")
    expect_equal(names(test2), c("matrix", "tree", "call", "subsets", "covar", "disparity"))
    ## Different results
    expect_equal(c(summary(test2)$obs), c(0.2, -1.7, -17.2))

    ## Works with between groups
    ## VCV && !loc
    test3 <- dispRity(data, metric = as.covar(sum.var.group, VCV = TRUE, loc = FALSE), between.groups = TRUE)
    expect_is(test3, "dispRity")
    expect_equal(names(test3), c("matrix", "tree", "call", "subsets", "covar", "disparity"))
    ## Different results
    # expect_equal(c(summary(test3)$obs), c(0.418, 0.539, 0.191))

    ## !VCV && loc
    test3 <- dispRity(data, metric = as.covar(sum.var.group, VCV = FALSE, loc = TRUE), between.groups = TRUE)
    expect_is(test3, "dispRity")
    expect_equal(names(test3), c("matrix", "tree", "call", "subsets", "covar", "disparity"))
    ## Different results
    expect_equal(c(summary(test3)$obs), c(3.12, 30.12, 33))

    ## VCV && loc
    test3 <- dispRity(data, metric = as.covar(sum.var.group, VCV = TRUE, loc = TRUE), between.groups = TRUE)
    expect_is(test3, "dispRity")
    expect_equal(names(test3), c("matrix", "tree", "call", "subsets", "covar", "disparity"))
    ## Different results
    expect_equal(c(summary(test3)$obs), c(-1.4, -16.9, -18.9))
})

test_that("example works", {

    ## Creating a dispRity
    data(charadriiformes)

    ## Creating a dispRity object from the charadriiformes model
    covar_data <- MCMCglmm.subsets(data       = charadriiformes$data,
                                   posteriors = charadriiformes$posteriors)

    ## Get one matrix and one covar matrix
    set.seed(1)
    one_matrix <- get.matrix(covar_data, subsets = 1)
    one_covar  <- get.covar(covar_data, subsets = 1, n = 1)[[1]][[1]]

    ## Measure the centroids on the covar matrix
    expect_equal_round(as.covar(centroids)(one_covar), c(0.06730570, 0.05852601, 0.01730266), digits = 6)
    ## Is the same as:
    expect_equal_round(as.covar(centroids)(one_covar), centroids(one_covar$VCV), digits = 6)
    

    ## Apply the measurement on a dispRity object:
    ## On the traitspace:
    expect_equal(c(summary(dispRity(covar_data, metric = c(sum, centroids)))$obs), c(71.2, 49.0, 52.1, 182.9, 182.9))
    ## On the covariance matrices:
    expect_equal(length(summary(dispRity(covar_data, metric = c(sum, as.covar(centroids))))$obs), 5)
    expect_equal(length(summary(dispRity(covar_data,
                         metric = c(sum, as.covar(centroids)),
                         centre = 100))$obs), 5)

    expect_equal(c(summary(dispRity(covar_data, metric = c(sum, as.covar(centroids))))$obs), c(0.375, 0.017, 0.112, 0.229, 0.029))
    ## The same but with additional options (centre = 100)
    expect_equal(c(summary(dispRity(covar_data,
                     metric = c(sum, as.covar(centroids)),
                     centre = 100))$obs), c(100.4, 100.0, 100.1, 100.2, 100.0))
})