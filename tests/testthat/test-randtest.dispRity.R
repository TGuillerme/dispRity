## Test
test_that("randtest.dispRity works", {

    set.seed(1)
    dummy_matrix <- matrix(rnorm(500), 100, 5)
    test_subset <- sample(1:100, 20)

    ## Sanitizing
    error <- capture_error(randtest.dispRity(data = "dummy_matrix", subsets = test_subset, metric = mean, replicates = 100, resample = TRUE, alter = "lesser"))
    expect_equal(error[[1]], "data must be of class matrix or dispRity.")

    error <- capture_error(randtest.dispRity(data = dummy_matrix, subsets = "test_subset", metric = mean, replicates = 100, resample = TRUE, alter = "lesser"))
    expect_equal(error[[1]], "Subsets must be a vector or a list of vector of integers or numeric values that can not exceed the number of rows in data.")

    error <- capture_error(randtest.dispRity(data = dummy_matrix, subsets = test_subset, metric = "mean", replicates = 100, resample = TRUE, alter = "lesser"))
    expect_equal(error[[1]], "metric must be of class function.")

    error <- capture_error(randtest.dispRity(data = dummy_matrix, subsets = test_subset, metric = mean, replicates = "100", resample = TRUE, alter = "lesser"))
    expect_equal(error[[1]], "replicates must be of class numeric or integer.")

    error <- capture_error(randtest.dispRity(data = dummy_matrix, subsets = test_subset, metric = mean, replicates = c(1,2.2), resample = TRUE, alter = "lesser"))
    expect_equal(error[[1]], "replicates must be a single numeric value.")

    error <- capture_error(randtest.dispRity(data = dummy_matrix, subsets = test_subset, metric = mean, replicates = -1, resample = TRUE, alter = "lesser"))
    expect_equal(error[[1]], "At least one replicate must be run.")

    error <- capture_error(randtest.dispRity(data = dummy_matrix, subsets = test_subset, metric = mean, replicates = 100, resample = "TRUE", alter = "lesser"))
    expect_equal(error[[1]], "resample must be of class logical.")

    error <- capture_error(randtest.dispRity(data = dummy_matrix, subsets = test_subset, metric = mean, replicates = 100, resample = TRUE, alter = "f"))
    expect_equal(error[[1]], "alter must be one of the following: two-sided, greater, lesser.")

    ## Testing whether the mean of a random subset
    ## is different than the means of 100 subsets
    dummy_test <- randtest.dispRity(
        data = dummy_matrix,
        subsets = test_subset,
        metric = mean, alter = "lesser")
    expect_is(dummy_test, c("dispRity", "randtest"))
    expect_equal(names(dummy_test), c("rep", "observed", "random", "call", "sim", "obs", "plot", "alter", "pvalue", "expvar", "n"))
    ## Normal print
    # print_results <- capture.output(dummy_test)
    # expect_equal(print_results, c("Monte-Carlo test"      , "Call: randtest.dispRity(data = dummy_matrix, subsets = test_subset, ", "    metric = mean)" , "", "Observation: -0.05355287 ", "", "Based on 100 replicates", "Simulated p-value: 0.5445545 ", "Alternative hypothesis: two-sided ", "", "Mean Normal residuals           Random mean ", "          -0.64347263            0.02131353 ", "      Random variance ", "           0.01353673 "))

    ## Normal summary
    expect_is(summary(dummy_test), c("summaryDefault", "table"))

    ## Normal plot
    # expect_null(plot(dummy_test))

    ## List of subsets
    set.seed(2)
    subsets_list <- replicate(5, sample(1:100, 20), simplify = FALSE)
    names(subsets_list) <- LETTERS[1:5]
    test_list <- randtest.dispRity(
        data = dummy_matrix,
        subsets = subsets_list,
        metric = c(mean, centroids))
    expect_is(test_list, c('dispRity', 'randtest'))
    expect_null(plot(test_list))
    test_sum <- summary(test_list)
    expect_is(test_sum, "matrix")
    expect_equal(dim(test_sum), c(5, 7))

    ## Applying this on dispRity objects
    data(disparity)
    test_list <- randtest.dispRity(disparity, alter = "greater")
    expect_is(test_list, c('dispRity', 'randtest'))
    expect_null(plot(test_list))
    test_sum <- summary(test_list)
    expect_is(test_sum, "matrix")
    expect_equal(dim(test_sum), c(7, 7))
    test_list <- test.dispRity(disparity, test = randtest, replicates = 500)
    expect_is(test_list, c('dispRity', 'randtest'))
    expect_null(plot(test_list))
    test_sum <- summary(test_list)
    expect_is(test_sum, "matrix")
    expect_equal(dim(test_sum), c(7, 7))

    ## Testing more complex subsets
    data(disparity)
    test_disparity2 <- randtest.dispRity(disparity,
        subsets = list("test1" = c(observed = "90"), "test2" = c(observed = "70", random = c("90", "70", "30"))))
    expect_is(test_disparity2, c('dispRity', 'randtest'))
    expect_equal(dim(summary(test_disparity2)), c(2, 7))
    expect_equal(names(test_disparity2), c("test1", "test2"))
    expect_null(plot(test_disparity2))
})
