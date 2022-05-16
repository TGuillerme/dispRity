# TESTING dispRity

#context("between.groups (inc. S3)")

test_that("dispRity works for between.groups metrics", {

    ## Some test metrics
    between.groups.simple <- function(matrix, matrix2) return(42)
    between.groups.complex <- function(matrix, matrix2) return(mean(matrix) - mean(matrix2))

    ## Testing data
    matrix <- do.call(rbind, list(matrix(1, 5, 5), matrix(2, 3, 5), matrix(3, 4, 5)))
    matrix_node <- do.call(rbind, list(matrix(3, 4, 5), matrix(2, 3, 5), matrix(1, 4, 5)))
    rownames(matrix) <- paste0("t", 1:12)
    test_tree <- stree(12, type = "right")
    rownames(matrix_node) <- paste0("n", 1:Nnode(test_tree))
    matrix_node <- rbind(matrix, matrix_node)
    test_tree$node.label <- paste0("n", 1:Nnode(test_tree))
    test_tree$edge.length <- rep(1, Nedge(test_tree))
    test_tree$root.time <- 12

    ## custom subsets
    custom <- custom.subsets(matrix, group = list(c(1:5), c(6:8), c(9:12)))
    chrono <- chrono.subsets(matrix, test_tree, method = "discrete", time = c(12, 8.1, 5.1, 0))

    ## Errors

    ## Serial is logical or list
    error <- capture_error(dispRity(matrix, metric = between.groups.simple, between.groups = "whatever"))
    expect_equal(error[[1]], "between.groups must be logical or a list of pairs of comparisons.")

    ## Wrong metric for between.groups data (mean)
    error <- capture_error(dispRity(matrix, metric = mean, between.groups = TRUE))
    expect_equal(error[[1]], "The provided metric (mean) cannot be applied between groups. \"between.groups\" metrics must have at least \"matrix\" and \"matrix2\" as inputs.")

    ## Has no between.groups dataset
    error <- capture_error(dispRity(matrix, metric = between.groups.simple, between.groups = TRUE))
    expect_equal(error[[1]], "The provided \"between.groups\" metric (between.groups.simple) cannot be applied to a dispRity object with no subsets. Use chrono.subsets or custom.subsets to create some.")

    ## Wrong metric for between.groups data (mean) (input list)
    error <- capture_error(dispRity(matrix, metric = mean, between.groups = list(c(1,2), c(2,3))))
    expect_equal(error[[1]], "The provided metric (mean) cannot be applied between groups. \"between.groups\" metrics must have at least \"matrix\" and \"matrix2\" as inputs.")

    ## Serial is logical or list
    error <- capture_error(dispRity(matrix, metric = between.groups.simple, between.groups = list(c(1,2), c(2,3))))
    expect_equal(error[[1]], "The provided list of groups (between.groups) must be a list of pairs of subsets in the data.")

    ## Warning for wrong names
    custom_wrong_names <- custom.subsets(matrix, group = list("A" = c(1:5), "B:" = c(6:8), ":::" = c(9:12)))
    warning <- capture_warning(dispRity(custom_wrong_names, metric = between.groups.simple, between.groups = TRUE))
    expect_equal(warning[[1]],  "The subset names: B:, ::: were changed to B;, ;;;. The \":\" character is reserved for between groups comparisons.")
    expect_warning(test <- dispRity(custom_wrong_names, metric = between.groups.simple, between.groups = TRUE))
    expect_equal(summary(test)$subsets, c("A:B;", "A:;;;", "B;:;;;"))

    ## Serial works for level 1
    test <- dispRity(custom, metric = between.groups.simple)
    expect_false(test$call$disparity$metrics$between.groups)
    test <- dispRity(custom, metric = between.groups.simple, between.groups = TRUE)
    expect_true(test$call$disparity$metrics$between.groups)

    ## Works for specified groups
    test <- dispRity(custom, metric = between.groups.simple, between.groups = list(c(1,2), c(1,1)))
    expect_equal(summary(test)$subsets, c("1:2", "1:1"))



    ## Custom normal
    test <- dispRity(custom, metric = between.groups.complex, between.groups = TRUE)
    expect_equal(capture.output(test)[4], "Disparity was calculated as: between.groups.complex between groups.")
    summary_results <- summary(test)
    expect_is(summary_results, "data.frame")
    expect_equal(dim(summary_results), c(3, 4))
    expect_equal(colnames(summary_results), c("subsets", "n_1", "n_2", "obs"))
    expect_equal(summary_results$subsets, c("1:2", "1:3", "2:3"))
    expect_equal(summary_results$obs, c(-1, -2, -1))
    expect_null(plot(test))

    ## Custom bootstrapped
    test <- dispRity(boot.matrix(custom, rarefaction = TRUE, bootstrap = 3), metric = between.groups.complex, between.groups = TRUE)
    expect_equal(capture.output(test)[5], "Disparity was calculated as: between.groups.complex between groups.")
    summary_results <- summary(test)
    expect_is(summary_results, "data.frame")
    expect_equal(dim(summary_results), c(8, 9))
    expect_equal(colnames(summary_results), c("subsets", "n_1", "n_2", "obs", "bs.median", "2.5%", "25%", "75%", "97.5%"))
    expect_equal(summary_results$subsets, c("1:2", "1:2", "1:2", "1:3", "1:3", "1:3", "2:3", "2:3"))
    expect_equal(summary_results$obs, c(-1, NA, NA, -2, NA, NA, -1, NA))
    
    ## Plots
    expect_null(plot(test, rarefaction = 3))
    expect_null(plot(test))
    expect_null(plot(test, observed = TRUE))


    ## Chrono normal
    test <- dispRity(chrono, metric = between.groups.complex, between.groups = TRUE)
    expect_equal(capture.output(test)[4], "Disparity was calculated as: between.groups.complex between groups.")
    summary_results <- summary(test)
    expect_is(summary_results, "data.frame")
    expect_equal(dim(summary_results), c(2, 4))
    expect_equal(colnames(summary_results), c("subsets", "n_1", "n_2", "obs"))
    expect_equal(summary_results$subsets, c("12 - 8.1:8.1 - 5.1", "8.1 - 5.1:5.1 - 0"))
    expect_equal(summary_results$obs, c(0.667, 1.167))
    expect_null(plot(test))

    ## Chrono bootstrapped + rare
    set.seed(1)
    test <- dispRity(boot.matrix(chrono, rarefaction = TRUE), metric = between.groups.complex, between.groups = TRUE)
    expect_equal(capture.output(test)[5], "Disparity was calculated as: between.groups.complex between groups.")
    summary_results <- summary(test)
    expect_is(summary_results, "data.frame")
    expect_equal(dim(summary_results), c(5, 9))
    expect_equal(colnames(summary_results), c("subsets", "n_1", "n_2", "obs", "bs.median", "2.5%", "25%", "75%", "97.5%"))
    expect_equal(summary_results$subsets, c("12 - 8.1:8.1 - 5.1", "8.1 - 5.1:5.1 - 0", "8.1 - 5.1:5.1 - 0", "8.1 - 5.1:5.1 - 0", "8.1 - 5.1:5.1 - 0"))
    expect_equal(summary_results$obs, c(0.667, 1.167, NA, NA, NA))
    expect_equal(summary_results$bs.median, c(0.667, 1.167, 1.133, 1.083, 1.000))

    ## plots
    expect_null(plot(test))
    expect_null(plot(test, elements = TRUE))

    ## Chrono time.slice
    chrono <- chrono.subsets(matrix_node, test_tree, method = "continuous", time = c(1,2,3,4,5), model = "acctran", verbose = FALSE)
    expect_warning(test <- dispRity(boot.matrix(chrono, 5), metric = between.groups.complex, between.groups = TRUE)) # Warning is just because bootstrapping < 3 elements
    results <- summary(test)
    expect_equal(dim(results), c(4, 9))
    expect_equal(colnames(results)[1:4], c("subsets", "n_1", "n_2", "obs"))
    expect_equal(results[,1], c("5:4", "4:3", "3:2","2:1"))
    expect_null(plot(test, elements = TRUE))

    ## Chrono bootstrapped
    data(BeckLee_mat99)
    data(BeckLee_tree)
    chrono <- chrono.subsets(BeckLee_mat99, BeckLee_tree, method = "continuous", model = "gradual.split", time = 6)
    test <- dispRity(boot.matrix(chrono, 10), metric = between.groups.complex, between.groups = TRUE)
    results <- summary(test, quantiles = 2)
    expect_equal(dim(results), c(5, 7))
    expect_equal(colnames(results)[1:4], c("subsets", "n_1", "n_2", "obs"))
    expect_equal(results[,1], c("133.51:106.81", "106.81:80.11", "80.11:53.4", "53.4:26.7", "26.7:0" ))
    expect_null(plot(test, elements = list(col = c("red", "blue"))))


    ## Mixing metric types
    level1_between.groups <- between.groups.complex
    level2_between.groups <- function(matrix, matrix2) {
        return(variances(matrix) - variances(matrix2))
    }

    ## Correctly handeling mixed metrics (and mixed options)
    test1 <- dispRity(custom, metric = level2_between.groups, between.groups = TRUE)
    the_warning <- capture_warning(test2 <- dispRity(test1, metric = mean, between.groups = FALSE))
    expect_warning(test2 <- dispRity(test1, metric = mean, between.groups = FALSE))
    expect_equal(the_warning[[1]], "The disparity calculation (metric = mean) is not calculated between groups (between.groups = FALSE) but the input data (test1) contained a between groups calculation. The metric is thus only applied to the groups (not between them). If this is not the desired behaviour, use the following option:\n    dispRity(..., between.groups = TRUE)")
    expect_equal(unlist(get.disparity(test2)), unlist(get.disparity(dispRity(custom, metric = mean))))

    test1 <- dispRity(custom, metric = mean, between.groups = FALSE)
    the_warning <- capture_warnings(test2 <- dispRity(test1, metric = level1_between.groups, between.groups = TRUE))
    expect_warning(test2 <- dispRity(test1, metric = level1_between.groups, between.groups = TRUE))
    expect_equal(the_warning[[1]], "The disparity calculation (metric = level1_between.groups) is calculated between groups (between.groups = TRUE) but the input data (test1) contained no between groups calculation. The metric is thus only applied between the groups (not to the previously calculated disparity). If this is not the desired behaviour, use the following option:\n    dispRity(..., between.groups = FALSE)")
    expect_equal(unlist(get.disparity(test2)), unlist(get.disparity(dispRity(custom, metric = level1_between.groups, between.groups = TRUE))))


    ## Handling multiple level metrics
    matrix1 <- matrix(1, 5, 4)
    matrix2 <- matrix(2, 10, 4)
    expect_warning(data <- custom.subsets(rbind(matrix1, matrix2), group = list(1:5, 6:15)))

    ## Works normally
    test <- dispRity(data, metric = point.dist, between.groups = TRUE)
    expect_equal(summary(test)$obs.median, 2)

    ## Works with additional level1
    test <- dispRity(data, metric = c(mean, point.dist), between.groups = TRUE)
    expect_equal(summary(test)$obs, 2)
    ## Works in any order
    test2 <- dispRity(data, metric = c(point.dist, mean), between.groups = TRUE)
    expect_equal(summary(test)$obs, summary(test2)$obs)

    ## Error if level 3 is not between.metric
    error <- capture_error(dispRity(data, metric = c(point.dist, var), between.groups = TRUE))
    expect_equal(error[[1]], "Impossible to apply a dimension-level 3 metric that is not a between group metric with a dimension-level1 or 2 metric that is. You can try to integrate that dimension-level 3 metric directly in the definition of the other metrics.")
    error <- capture_error(dispRity(data, metric = c(var, point.dist), between.groups = TRUE))
    expect_equal(error[[1]], "Impossible to apply a dimension-level 3 metric that is not a between group metric with a dimension-level1 or 2 metric that is. You can try to integrate that dimension-level 3 metric directly in the definition of the other metrics.")

    ## Between groups with tree metrics
    tree.diff <- function(matrix, matrix2, tree) {
        return(sum(edge.length.tree(matrix, tree)) - sum(edge.length.tree(matrix2, tree)))
    }
    data(BeckLee_mat99)
    data(BeckLee_tree)
    data <- custom.subsets(BeckLee_mat99, group = crown.stem(BeckLee_tree, inc.nodes = TRUE))
    test <- dispRity(data, metric = tree.diff, between.groups = list(c(1,2), c(2,1)), tree = BeckLee_tree)
    expect_is(test, "dispRity")
    expect_equal_round(test$disparity[[1]][[1]][1], 3307.54, 2)
    expect_equal_round(test$disparity[[2]][[1]][1], -3307.54, 2)
})
