 # - 1. metrics can now have `dist.help` arguments that intake a function that will run some pre-calculations. For example, this function can be `vegan::vegdist`.
 # - 2. detect the need for RAM help in `get.dispRity.metric.handle`
 # - 3. compute heavy calculations at the whole data level in `dispRity` using the `dist.help` function before the `lapply_loop`
 # - 4. store the calculations in `data` similarly as tree as `dist.helper`
 # - 5. run the metrics using a potential `dist.helper` similarly as tree.


dist.with.help <- function(matrix, method = "euclidean", dist.helper = vegan::vegdist) {
    ## Check for distance
    distances <- check.dist.matrix(matrix, method = method)[[1]]
    ## Return distances
    return(as.vector(distances))
}
dist.no.help <- function(matrix, method = "euclidean", dist.helper = FALSE) {
    ## Check for distance
    distances <- check.dist.matrix(matrix, method = method)[[1]]
    ## Return distances
    return(as.vector(distances))
}
dist.no.help2 <- function(matrix, method = "euclidean", dist.helper = NULL) {
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
    expect_equal(names(test), c("type", "tree", "dist.help", "reduce.dist"))
    expect_is(test$dist.help, "list")
    expect_is(test$dist.help[[1]], "matrix")

    ## Get the help from get.dispRity.metric.handle
    # test <- get.dispRity.metric.handle(metric = dist.with.help, match_call = list(), data = data, tree = NULL)
    # expect_is(test, "list")
    # expect_equal(names(test), c("levels", "between.groups", "tree.metrics", "dist.help"))
    # expect_is(test$dist.help, "list")
    # expect_is(test$dist.help[[1]], "matrix")

    test <- get.dispRity.metric.handle(metric = pairwise.dist, match_call = list(), data = data, tree = NULL)
    expect_is(test, "list")
    expect_equal(names(test), c("levels", "between.groups", "tree.metrics", "dist.help"))
    expect_null(test$dist.help)

    test <- get.dispRity.metric.handle(metric = dist.no.help, match_call = list(), data = data, tree = NULL)
    expect_is(test, "list")
    expect_equal(names(test), c("levels", "between.groups", "tree.metrics", "dist.help"))
    expect_null(test$dist.help)
})

test_that("reduce.checks works", {
    matrix <- matrix(rnorm(90), 9, 10)
    dist_mat <- as.matrix(dist(matrix))

    expect_is(reduce.checks(mean), "function")
    expect_null(reduce.checks(NULL))

    ## No reduction of checks
    expect_equal(pairwise.dist(matrix), reduce.checks(fun = pairwise.dist, reduce.dist = NULL)(matrix))
    ## Removing checks (matrix is already distance)
    expect_equal(pairwise.dist(matrix), reduce.checks(fun = pairwise.dist, reduce.dist = TRUE)(dist_mat))
    expect_equal(neighbours(matrix), reduce.checks(fun = neighbours, reduce.dist = TRUE)(dist_mat))
    expect_equal(span.tree.length(matrix), reduce.checks(fun = span.tree.length, reduce.dist = TRUE)(dist_mat))
    expect_equal(func.eve(matrix), reduce.checks(fun = func.eve, reduce.dist = TRUE)(dist_mat))
    expect_equal(count.neighbours(matrix), reduce.checks(fun = count.neighbours, reduce.dist = TRUE)(dist_mat))

    ## Removing other checks
    expect_equal(angles(matrix), reduce.checks(angles, reduce.dist = NULL)(matrix))
    expect_equal(deviations(matrix), reduce.checks(deviations, reduce.dist = NULL)(matrix))

    ## Works with options
    expect_equal(count.neighbours(matrix, radius = 2), reduce.checks(fun = count.neighbours, reduce.dist = NULL)(dist_mat, radius = 2))
    expect_equal(count.neighbours(matrix, radius = 0.1), reduce.checks(fun = count.neighbours, reduce.dist = NULL)(dist_mat, radius = 0.1))
})

test_that("general structure works", {

    set.seed(1)
    data <- matrix(rnorm(90), 9, 10, dimnames = list(letters[1:9]))

    test <- dispRity(data = data, metric = pairwise.dist)
    check.class(test, "dispRity")
    expect_equal(length(test$disparity[[1]][[1]]), 36)
    expect_equal(summary(test)$obs, 4.041)

    test <- dispRity(data = data, metric = pairwise.dist, dist.helper = vegan::vegdist)
    check.class(test, "dispRity")
    expect_equal(length(test$disparity[[1]][[1]]), 36)
    expect_equal(summary(test)$obs, 4.041)

    ## Working with dist.helper being a matrix or a list
    dist_matrix <- vegan::vegdist(data, method = "euclidean")
    test <- dispRity(data = data, metric = pairwise.dist, dist.helper = dist_matrix)
    check.class(test, "dispRity")
    expect_equal(length(test$disparity[[1]][[1]]), 36)
    expect_equal(summary(test)$obs, 4.041)

    test <- dispRity(data = data, metric = pairwise.dist, dist.helper = list(dist_matrix))
    expect_equal(summary(test)$obs, 4.041)

    ## Errors (wrong matrices)
    ## Errors from make.metric
    error <- capture_error(test <- dispRity(data = data, metric = pairwise.dist, dist.helper = data))
    expect_equal(error[[1]], "dist.helper argument must be a distance matrix (or list of them) or a function to generate a distance matrix.")
    error <- capture_error(test <- dispRity(data = data, metric = pairwise.dist, dist.helper = rnorm))
    expect_equal(error[[1]], "dist.helper argument must be a distance matrix (or list of them) or a function to generate a distance matrix.")

    ## Working with dist.helper options recycled from "metric"
    test <- dispRity(data = data, metric = pairwise.dist, method = "manhattan")
    expect_equal(summary(test)$obs, 10.01)
    test <- dispRity(data = data, metric = pairwise.dist, method = "manhattan", dist.helper = vegan::vegdist)
    expect_equal(summary(test)$obs, 10.01)
    dist_matrix <- vegan::vegdist(data, method = "manhattan")
    test <- dispRity(data = data, metric = pairwise.dist, method = "manhattan", dist.helper = dist_matrix)
    expect_equal(summary(test)$obs, 10.01)

    ## Working with multiple metrics
    error <- capture_error(test <- dispRity(data = data, metric = c(mean, pairwise.dist), dist.helper = dist_matrix))
    expect_equal(error[[1]], "dist.help can only be used for one metric. You can try combine the 2 metrics together into one or calculate disparity step by step. For example:\ndispRity(dispRity(data, metric = level2.metric), metric = level1.metric)")
    error <- capture_error(test <- dispRity(data = data, metric = c(mean, pairwise.dist), dist.helper = vegan::vegdist))
    expect_equal(error[[1]], "dist.help can only be used for one metric. You can try combine the 2 metrics together into one or calculate disparity step by step. For example:\ndispRity(dispRity(data, metric = level2.metric), metric = level1.metric)")

    ## Working with metrics that are user designed.
    dist.of.pairs1 <- function(matrix, ...) {
        return(as.vector(dist(matrix)))
    }
    dist.of.pairs2 <- function(matrix, ...) {
        distances <- stats::dist(matrix)
        return(as.vector(distances))
    }

    test <- dispRity(data = data, metric = dist.of.pairs1)
    expect_equal(summary(test)$obs, 4.041)

    test <- dispRity(data = data, metric = dist.of.pairs2, dist.helper = stats::dist)
    expect_equal(summary(test)$obs, 4.041)

    test <- dispRity(data = data, metric = dist.of.pairs1, dist.helper = dist)
    expect_equal(summary(test)$obs, 4.041)

    ## Works but actually doesn't use helper
    test <- dispRity(data = data, metric = dist.of.pairs2, dist.helper = dist)
    expect_equal(summary(test)$obs, 4.041)
})

test_that("works with bootstraps", {

    data(BeckLee_mat99)
    data(BeckLee_tree)
    groups <- chrono.subsets(BeckLee_mat99, tree = BeckLee_tree, time = 10, method = "continuous", model = "acctran")
    bs_data <- boot.matrix(groups, bootstraps = 500)

    test <- dispRity(data = bs_data, metric = pairwise.dist)
    check.class(test, "dispRity")
    expect_equal(dim(summary(test)), c(10, 8))
    expect_equal(summary(test)$obs.median, c(2.472, 2.537, 2.623, 2.723, 2.750, 2.785, 2.841, 2.867, 2.867, 2.867))

    test <- dispRity(data = bs_data, metric = pairwise.dist, dist.helper = stats::dist)
    check.class(test, "dispRity")
    expect_equal(dim(summary(test)), c(10, 8))
    expect_equal(summary(test)$obs.median, c(2.472, 2.537, 2.623, 2.723, 2.750, 2.785, 2.841, 2.867, 2.867, 2.867))

    dist_matrix <- dist(BeckLee_mat99)
    test <- dispRity(data = bs_data, metric = pairwise.dist, dist.helper = dist_matrix)
    check.class(test, "dispRity")
    expect_equal(dim(summary(test)), c(10, 8))
    expect_equal(summary(test)$obs.median, c(2.472, 2.537, 2.623, 2.723, 2.750, 2.785, 2.841, 2.867, 2.867, 2.867))


# test <- microbenchmark("no help"       = dispRity(bs_data, metric = pairwise.dist),
#                        "with help"     = dispRity(bs_data, metric = pairwise.dist, dist.helper = vegan::vegdist),
#                        "with pre calc" = dispRity(bs_data, metric = pairwise.dist, dist.helper = dist_matrix))
# plot(test)

})


test_that("works with trees", {

    metric.pairdist <- function(matrix, tree, ...) {
        distances <- dist(matrix)
        return(sum(distances)/sum(tree$edge.length))
    }

    metric.pairdist2 <- function(matrix, tree, ...) {
        morpho_distances <- dist(matrix)
        return(sum(morpho_distances)/sum(tree$edge.length))
    }

    set.seed(1)
    tree <- stree(7, type = "right")
    tree$edge.length <- rep(1, 7+6)
    tree$node.label <- letters[1:6]
    tree$tip.label <- LETTERS[1:7]
    ## An empty matrix (with the right elements)
    matrix <- matrix(rnorm((7+6)*2), nrow = 7+6, ncol = 2)
    rownames(matrix) <- c(tree$tip.label, tree$node.label)

    ## Simple test
    test <- dispRity(data = matrix, metric = metric.pairdist, tree = tree)
    expect_equal(c(test$disparity[[1]][[1]]), metric.pairdist(matrix, tree))

    ## Test with helper
    test <- dispRity(data = matrix, metric = metric.pairdist, tree = tree, dist.helper = dist)
    expect_equal(c(test$disparity[[1]][[1]]), metric.pairdist(matrix, tree))

    ## Test with helper
    test <- dispRity(data = matrix, metric = metric.pairdist2, tree = tree, dist.helper = dist)
    expect_equal(c(test$disparity[[1]][[1]]), metric.pairdist(matrix, tree))
})


# test_that("TODO: works with between groups", {

#     dist.difference <- function(matrix, matrix2, ...) {
#         return(sum(dist(matrix)) - sum(dist(matrix2)))
#     }

#     matrix <- data[c(1:5),]
#     matrix2 <- data[c(6:8),]


#     ## Testing data
#     set.seed(1)
#     data <- do.call(rbind, list(matrix(1, 5, 5), matrix(2, 3, 5), matrix(rnorm(4*5), 4, 5)))
#     rownames(data) <- letters[1:12]

#     ## custom subsets
#     custom <- custom.subsets(data, group = list(c(1:5), c(6:8), c(9:12)))
#     test <- dispRity(custom, metric = dist.difference, between.groups = TRUE)
#     expect_equal(capture.output(test)[4], "Disparity was calculated as: dist.difference between groups.")
#     summary_results <- summary(test)
#     expect_equal(summary_results$subsets, c("1:2", "1:3", "2:3"))
#     expect_equal(summary_results$obs[1], 0)
#     expect_equal(summary_results$obs[2], -17.3)
#     expect_equal(summary_results$obs[3], -17.3)

#     error <- capture_warning(test <- dispRity(custom, metric = dist.difference, between.groups = TRUE, dist.helper = dist))
#     expect_equal(error[[1]], "dist.helper is not yet implemented for between.groups metrics.")

#     ##TODO!

#     test <- dispRity(custom, metric = dist.difference2, between.groups = TRUE, dist.helper = dist)
#     expect_equal(capture.output(test)[4], "Disparity was calculated as: dist.difference between groups.")
#     summary_results <- summary(test)
#     expect_equal(summary_results$subsets, c("1:2", "1:3", "2:3"))
#     expect_equal(summary_results$obs[1], 0)
#     expect_equal(summary_results$obs[2], -17.3)
#     expect_equal(summary_results$obs[3], -17.3)
# })
