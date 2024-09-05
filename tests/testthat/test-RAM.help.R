 # - 1. metrics can now have `RAM.help` arguments that intake a function that will run some pre-calculations. For example, this function can be `vegan::vegdist`.
 # - 2. detect the need for RAM help in `get.dispRity.metric.handle`
 # - 3. compute heavy calculations at the whole data level in `dispRity` using the `RAM.help` function before the `lapply_loop`
 # - 4. store the calculations in `data` similarly as tree as `RAM.helper`
 # - 5. run the metrics using a potential `RAM.helper` similarly as tree.


dist.with.help <- function(matrix, method = "euclidean", RAM.helper = vegan::vegdist) {
    ## Check for distance
    distances <- check.dist.matrix(matrix, method = method)[[1]]
    ## Return distances
    return(as.vector(distances))
}
dist.no.help <- function(matrix, method = "euclidean", RAM.helper = FALSE) {
    ## Check for distance
    distances <- check.dist.matrix(matrix, method = method)[[1]]
    ## Return distances
    return(as.vector(distances))
}
dist.no.help2 <- function(matrix, method = "euclidean", RAM.helper = NULL) {
    ## Check for distance
    distances <- check.dist.matrix(matrix, method = method)[[1]]
    ## Return distances
    return(as.vector(distances))
}

test_that("check.get.help works", {
    expect_false(check.get.help(pairwise.dist))
    expect_false(check.get.help(dist.no.help))
    expect_false(check.get.help(dist.no.help2))
    expect_true(check.get.help(dist.with.help))
})

test_that("make.metric handles help", {

    data <- make.dispRity(data = matrix(rnorm(90), 9, 10))

    ## Get the help from make.metric
    test <- make.metric(fun = dist.with.help, data.dim = data, get.help = TRUE, silent = TRUE)
    expect_is(test, "list")
    expect_equal(names(test), c("type", "tree", "RAM.help"))
    expect_is(test$RAM.help, "list")
    expect_is(test$RAM.help[[1]], "matrix")

    ## Get the help from get.dispRity.metric.handle
    test <- get.dispRity.metric.handle(metric = dist.with.help, match_call = list(), data = data, tree = NULL)
    expect_is(test, "list")
    expect_equal(names(test), c("levels", "between.groups", "tree.metrics", "RAM.help"))
    expect_is(test$RAM.help, "list")
    expect_is(test$RAM.help[[1]], "matrix")

    test <- get.dispRity.metric.handle(metric = pairwise.dist, match_call = list(), data = data, tree = NULL)
    expect_is(test, "list")
    expect_equal(names(test), c("levels", "between.groups", "tree.metrics", "RAM.help"))
    expect_null(test$RAM.help)

    test <- get.dispRity.metric.handle(metric = dist.no.help, match_call = list(), data = data, tree = NULL)
    expect_is(test, "list")
    expect_equal(names(test), c("levels", "between.groups", "tree.metrics", "RAM.help"))
    expect_null(test$RAM.help)
})

test_that("reduce.checks works", {
    matrix <- matrix(rnorm(90), 9, 10)
    dist_mat <- dist(matrix) ## Keep as distance!

    expect_is(reduce.checks(mean), "function")
    expect_null(reduce.checks(NULL))

    ## No reduction of checks
    expect_equal(pairwise.dist(matrix), reduce.checks(fun = pairwise.dist, data = matrix, get.help = FALSE)(matrix))
    ## Removing checks (matrix is already distance)
    expect_equal(pairwise.dist(matrix), reduce.checks(fun = pairwise.dist, data = dist_mat, get.help = TRUE)(dist_mat))
    expect_equal(neighbours(matrix), reduce.checks(fun = neighbours, data = dist_mat, get.help = TRUE)(dist_mat))
    expect_equal(span.tree.length(matrix), reduce.checks(fun = span.tree.length, data = dist_mat, get.help = TRUE)(dist_mat))
    expect_equal(func.eve(matrix), reduce.checks(fun = func.eve, data = dist_mat, get.help = TRUE)(dist_mat))
    expect_equal(count.neighbours(matrix), reduce.checks(fun = count.neighbours, data = dist_mat, get.help = TRUE)(dist_mat))

    ## Removing other checks
    expect_equal(angles(matrix), reduce.checks(angles, data = matrix, get.help = FALSE)(matrix))
    expect_equal(deviations(matrix), reduce.checks(deviations, data = matrix, get.help = FALSE)(matrix))

    ## Works with options
    expect_equal(count.neighbours(matrix, radius = 2), reduce.checks(fun = count.neighbours, data = dist_mat, get.help = TRUE)(dist_mat, radius = 2))
    expect_equal(count.neighbours(matrix, radius = 0.1), reduce.checks(fun = count.neighbours, data = dist_mat, get.help = TRUE)(dist_mat, radius = 0.1))
})

test_that("general structure works", {

    ## TODO:
    # Make sure RAM_help is passed correctly to decompose

    set.seed(1)
    data <- matrix(rnorm(90), 9, 10, dimnames = list(letters[1:9]))

    start <- Sys.time()
    test <- dispRity(data = data, metric = pairwise.dist)
    end <- Sys.time()
    no_help_time <- end-start
    check.class(test, "dispRity")
    expect_equal(length(test$disparity[[1]][[1]]), 36)
    expect_equal(summary(test)$obs, 4.041)

    start <- Sys.time()
    test <- dispRity(data = data, metric = pairwise.dist, RAM.helper = vegan::vegdist)
    end <- Sys.time()
    with_help_time <- end-start
    check.class(test, "dispRity")
    expect_equal(length(test$disparity[[1]][[1]]), 36)
    expect_equal(summary(test)$obs, 4.041)

    ## Help is faster (roughly 10 times? - NOPE?)
    # library(microbenchmark)
    # test <- microbenchmark("with help"    = dispRity(data = data, metric = pairwise.dist, RAM.helper = vegan::vegdist),
    #                        "without help" = dispRity(data = data, metric = pairwise.dist))
    # plot(test, main = "basic test")

    ## Working with RAM.helper being a matrix or a list
    dist_matrix <- vegan::vegdist(data, method = "euclidean")
    start <- Sys.time()
    test <- dispRity(data = data, metric = pairwise.dist, RAM.helper = dist_matrix)
    end <- Sys.time()
    with_help_time <- end-start
    check.class(test, "dispRity")
    expect_equal(length(test$disparity[[1]][[1]]), 36)
    expect_equal(summary(test)$obs, 4.041)
    # expect_gt(no_help_time, with_help_time)

    start <- Sys.time()
    test <- dispRity(data = data, metric = pairwise.dist, RAM.helper = list(dist_matrix))
    end <- Sys.time()
    with_help_time <- end-start
    expect_equal(summary(test)$obs, 4.041)
    # expect_gt(no_help_time, with_help_time)


    ## Errors (wrong matrices)
    ## Errors from make.metric
    expect_warning(error <- capture_error(test <- dispRity(data = data, metric = pairwise.dist, RAM.helper = data)))
    expect_equal(error[[1]], "RAM.helper argument must be a distance matrix (or list of them) or a function to generate a distance matrix.")
    error <- capture_error(test <- dispRity(data = data, metric = pairwise.dist, RAM.helper = rnorm))
    expect_equal(error[[1]], "RAM.helper argument must be a distance matrix (or list of them) or a function to generate a distance matrix.")

    error <- capture_error(test <- dispRity(data = data, metric = pairwise.dist, RAM.helper = list(dist_matrix, dist_matrix)))
    error <- capture_error(test <- dispRity(data = data, metric = pairwise.dist, RAM.helper = dist(matrix(rnorm(90), 9, 10, dimnames = list(LETTERS[1:9])))))


    ## Working with RAM.helper options recycled from "metric"
    test <- dispRity(data = data, metric = pairwise.dist, method = "manhattan")
    expect_equal(summary(test)$obs, 10.01)
    test <- dispRity(data = data, metric = pairwise.dist, method = "manhattan", RAM.helper = vegan::vegdist)
    expect_equal(summary(test)$obs, 10.01)



    ## Working with RAM.helper designed in the metric
    dist.with.help <- function(matrix, method = "euclidean", RAM.helper = vegan::vegdist) {
        ## Check for distance
        distances <- check.dist.matrix(matrix, method = method)[[1]]
        ## Return distances
        return(as.vector(distances))
    }

    start <- Sys.time()
    test <- dispRity(data = data, metric = pairwise.dist)
    end <- Sys.time()
    no_help_time <- end-start
    expect_equal(summary(test)$obs, 4.041)
    
    start <- Sys.time()
    test <- dispRity(data = data, metric = dist.with.help)
    end <- Sys.time()
    with_help_time <- end-start
    expect_equal(summary(test)$obs, 4.041)
    expect_gt(no_help_time, with_help_time)

    ## Also handles optionals
    test <- dispRity(data = data, metric = pairwise.dist, method = "manhattan")
    expect_equal(summary(test)$obs, 10.01)
    test <- dispRity(data = data, metric = dist.with.help, method = "manhattan")
    expect_equal(summary(test)$obs, 10.01)


})

test_that("works with bootstraps", {

    # data(BeckLee_mat99)
    # data(BeckLee_tree)
    # groups <- chrono.subsets(BeckLee_mat99, tree = BeckLee_tree, time = 10, method = "continuous", model = "acctran")

    # start <- Sys.time()
    # test <- dispRity(data = boot.matrix(groups, 5000), metric = c(median, pairwise.dist))
    # end <- Sys.time()
    # check.class(test, "dispRity")
    # expect_equal(dim(summary(test)), c(10, 8))
    # expect_equal(summary(test)$obs, c(2.472, 2.537, 2.623, 2.723, 2.750, 2.785, 2.841, 2.867, 2.867, 2.867))
    # no_help_time <- end-start

    # start <- Sys.time()
    # test <- dispRity(data = boot.matrix(groups, 5000), metric = c(median, dist.with.help))
    # end <- Sys.time()
    # check.class(test, "dispRity")
    # expect_equal(dim(summary(test)), c(10, 8))
    # expect_equal(summary(test)$obs, c(2.472, 2.537, 2.623, 2.723, 2.750, 2.785, 2.841, 2.867, 2.867, 2.867))
    # with_help_time <- end-start


    # ## This should take the helper into account!
    # start <- Sys.time()
    # test <- dispRity(data = boot.matrix(groups, 100), metric = c(median, pairwise.dist), RAM.helper = stats::dist)
    # end <- Sys.time()
    # check.class(test, "dispRity")
    # expect_equal(dim(summary(test)), c(10, 8))
    # expect_equal(summary(test)$obs, c(2.472, 2.537, 2.623, 2.723, 2.750, 2.785, 2.841, 2.867, 2.867, 2.867))
    # with_help_time2 <- end-start

})

test_that("works with trees", {
    
})

test_that("works with covar", {
    
})

test_that("works with between groups", {

})

test_that("works with multiple matrices", {

})



test_that("works with dispRity multi", {

})

