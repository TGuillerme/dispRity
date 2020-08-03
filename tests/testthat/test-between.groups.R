# TESTING dispRity

context("between.groups (inc. S3)")

test_that("dispRity works for between.groups metrics", {

    ## Some test metrics
    between.groups.simple <- function(matrix, matrix2) return(42)
    between.groups.complex <- function(matrix, matrix2) return(mean(matrix) - mean(matrix2))

    ## Testing data
    matrix <- do.call(rbind, list(matrix(1, 5, 5), matrix(2, 3, 5), matrix(3, 4, 5)))
    rownames(matrix) <- paste0("t", 1:12)
    test_tree <- stree(12, type = "right")
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


    ## Serial works for level 1
    test <- dispRity(custom, metric = between.groups.simple)
    expect_false(test$call$disparity$metrics$between.groups)
    test <- dispRity(custom, metric = between.groups.simple, between.groups = TRUE)
    expect_true(test$call$disparity$metrics$between.groups)

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
    expect_null(plot(test, rarefaction = 3))


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
    expect_null(plot(test))

    ## Chrono time.slice
    # data(BeckLee_mat99)
    # data(BeckLee_tree)
    # chrono <- chrono.subsets(BeckLee_mat99, BeckLee_tree, method = "continuous", time = 11, model = "acctran", verbose = FALSE)
    # test <- dispRity(boot.matrix(chrono, 5), metric = between.groups.complex, between.groups = TRUE)
    # expect_equal(capture.output(test)[5], "Disparity was calculated as: between.groups.complex between groups.")
    # summary_results <- summary(test)
    # expect_is(summary_results, "data.frame")
    # expect_equal(dim(summary_results), c(10, 9))
    # expect_equal(colnames(summary_results), c("subsets", "n_1", "n_2", "obs", "bs.median", "2.5%", "25%", "75%", "97.5%"))
    # expect_null(plot(test))


    ## Chrono bootstrapped


    ## Decompose with between.groups then normal metrics


    ## Decompose with normal metrics then between.groups





})