context("test.metric")

## Test
test_that("test.metric works", {

    ## Data load
    data(disparity)
    space <- space.maker(100, 2, runif)

    ## Sanitizing test
    # (data, metric, ..., shifts, shift.options, model, replicates = 3, steps = 10, dimensions, verbose = FALSE)
    expect_error(test.metric("space", metric = c(prod, ranges), replicates = 1, steps = 10, dimensions = 2, verbose = FALSE))
    expect_error(test.metric(space, metric = "c(prod, ranges)", replicates = 1, steps = 10, dimensions = 2, verbose = FALSE))
    expect_error(test.metric(space, metric = c(prod, ranges), replicates = "1", steps = 10, dimensions = 2, verbose = FALSE))
    expect_error(test.metric(space, metric = c(prod, ranges), replicates = 1, steps = list(10), dimensions = 2, verbose = FALSE))
    expect_error(test.metric(space, metric = c(prod, ranges), replicates = 1, steps = 10, dimensions = mean, verbose = FALSE))
    expect_error(test.metric(space, metric = c(prod, ranges), replicates = 1, steps = 10, dimensions = 2, verbose = "FALSE"))

    ## A simple test with only 1 replicate for two shifts (random and size):
    test <- test.metric(space, metric = c(prod, ranges), replicates = 1, shifts = c("random", "size")) 
    expect_is(test, c("dispRity", "test.metric"))
    expect_equal(names(test), c("call", "results", "models"))
    expect_equal(names(test$results), c("random", "size.inner", "size.outer"))
    expect_is(test$results[[1]], "data.frame")

    ## Print works
    print_out <- capture.output(test)
    expect_equal(print_out, 
        c("Metric testing:"                                             ,
          "The following metric was tested: c(prod, ranges)."           ,
          "The test was run on the random, size shifts for 1 replicate.",
          "Use summary($) or plot($) for more details. "                ,
          "Use summary(item) or plot(item) for more details. "          ,
          "Use summary(value) or plot(value) for more details."  ))


    ## Summarising basic works
    expect_equal(dim(summary(test)), c(3, 10))
    expect_is(summary(test), "matrix")
    expect_equal(rownames(summary(test)), c("random", "size.inner", "size.outer"))
    expect_equal(colnames(summary(test)), paste0(seq(from = 10, to = 100, by = 10), "%"))

    ## Plot works
    expect_null(plot(test))

    # More complex example
    test <- test.metric(space, metric = c(sum, variances), steps = 5,
                        shifts = c("random", "size", "density", "position"), verbose = FALSE)
    expect_is(test, c("dispRity", "test.metric"))
    expect_equal(names(test), c("call", "results", "models"))
    expect_equal(names(test$results), c("random", "size.inner", "size.outer", "density.higher", "density.lower", "position.bottom", "position.top"))
    expect_is(test$results[[1]], "data.frame")
    expect_is(test$models[[1]], "lm")

    ## Print works
    print_out <- capture.output(test)
    expect_equal(print_out, 
        c("Metric testing:",
          "The following metric was tested: c(sum, variances).",
          "The test was run on the random, size, density, position shifts for 3 replicates using the following model:",
          "lm(disparity ~ reduction, data = data)",
          "Use summary($) or plot($) for more details. ",
          "Use summary(item) or plot(item) for more details. " ,
          "Use summary(value) or plot(value) for more details."))


    ## Summarising basic works
    expect_equal(dim(summary(test)), c(7, 8))
    expect_is(summary(test), "matrix")
    expect_equal(rownames(summary(test)), names(test$results))
    expect_equal(colnames(summary(test)), c("10%", "32.5%", "55%", "77.5%", "100%", "slope", "p_value", "R^2(adj)"))

    ## Plot works
    expect_null(plot(test))


    ## Applying the test directly on a disparity object
    test <- test.metric(disparity, shifts = "size", verbose = FALSE)
    expect_is(test, c("dispRity", "test.metric"))
    expect_equal(names(test), c("call", "results", "models"))
    expect_equal(names(test$results), c("size.inner", "size.outer"))
    expect_is(test$results[[1]], "data.frame")
    expect_is(test$models[[1]], "lm")

    ## Print works
    print_out <- capture.output(test)
    expect_equal(print_out, 
        c("Metric testing:",
          "The following metric was tested: c(median, centroids).",
          "The test was run on the size shift for 3 replicates using the following model:",
          "lm(disparity ~ reduction, data = data)",
          "Use summary($) or plot($) for more details. ",
          "Use summary(item) or plot(item) for more details. " ,
          "Use summary(value) or plot(value) for more details."))


    ## Summarising basic works
    expect_equal(dim(summary(test)), c(2, 13))
    expect_is(summary(test), "matrix")
    expect_equal(rownames(summary(test)), names(test$results))
    expect_equal(colnames(summary(test)), c(paste0(seq(from = 10, to = 100, by = 10), "%"), "slope", "p_value", "R^2(adj)"))
    

    ## Plot works
    expect_null(plot(test))

})