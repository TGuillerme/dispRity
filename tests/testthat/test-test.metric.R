#context("test.metric")

## Test
test_that("test.metric works", {

    ## Data load
    data(disparity)
    space <- space.maker(100, 2, runif)

    ## Sanitizing test
    # (data, metric, ..., shifts, shift.options, model, replicates = 3, steps = 10, dimensions, verbose = FALSE)
    expect_error(test.metric("space", metric = c(prod, ranges), replicates = 1, steps = 10, dimensions = 2, verbose = FALSE))
    expect_error(test.metric(space, metric = "c(prod, ranges)", replicates = 1, steps = 10, dimensions = 2, verbose = FALSE))
    expect_error(test.metric(space, metric = c(prod, ranges), replicates = "1", steps = 10, dimensions = 2, verbose = FALSE, shifts = c("random", "evenness")))
    expect_error(test.metric(space, metric = c(prod, ranges), replicates = 1, steps = list(10), dimensions = 2, verbose = FALSE, shifts = c("random", "evenness")))
    expect_error(test.metric(space, metric = c(prod, ranges), replicates = 1, steps = 10, dimensions = 2, verbose = "FALSE", shifts = c("random", "evenness")))

    error <- capture_error(test.metric(space, metric = c(prod, ranges), shifts = c("random", "evenness"), shift.options = 8))
    expect_equal(error[[1]], "shift.options must be of class list.")
    error <- capture_error(test.metric(space, metric = c(prod, ranges), shifts = c("random", "evenness"),replicates = 10, model = 8))
    expect_equal(error[[1]], "model must be of class function.")
    error <- capture_error(test.metric(space, metric = c(prod, ranges), shifts = c("random", "evenness"),replicates = 10, model = mean))
    expect_equal(error[[1]], "model function argument can only take \"data\" as an argument.")


    ## A simple test with only 1 replicate for two shifts (random and size):
    test <- test.metric(space, metric = c(prod, ranges), replicates = 1, shifts = c("random", "size"), shift.options = list(tunning = c(max = 1000000)))
    expect_is(test, c("dispRity", "test.metric"))
    expect_equal(names(test), c("call", "results", "models", "saved_steps"))
    expect_equal(names(test$results), c("random", "size.increase", "size.hollowness"))
    expect_is(test$results[[1]], "data.frame")
    expect_null(test$saved_steps)

    ## Print works
    print_out <- capture.output(test)
    expect_equal(print_out, 
        c("Metric testing:"                                             ,
          "The following metric was tested: c(prod, ranges)."           ,
          "The test was run on the random, size shifts for 1 replicate.",
          "Use summary(x) or plot(x) for more details."))                #,
          # "Use summary(item) or plot(item) for more details."          ,
          # "Use summary(value) or plot(value) for more details."  ))

    ## Verbose works
    test <- test.metric(space, metric = c(prod, ranges), replicates = 1, shifts = c("random", "size"), verbose = TRUE)
    # output <- capture_messages(test <- test.metric(space, metric = c(prod, ranges), replicates = 1, shifts = c("random", "size"), verbose = TRUE))
    # expect_equal(output, c("Running the space reductions:", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", "Done.\n"                      , "Calculating disparity:", ".", ".", ".", "Done.\n"))

    ## Summarising basic works
    expect_equal(dim(summary(test)), c(3, 10))
    expect_is(summary(test), "matrix")
    expect_equal(rownames(summary(test)), c("random", "size.increase", "size.hollowness"))
    expect_equal(colnames(summary(test)), paste0(seq(from = 10, to = 100, by = 10), "%"))

    ## Plot works
    expect_null(plot(test))
    expect_null(plot(test, col = "pink"))
    expect_null(plot(test, specific.args = list(legend = FALSE)))
    expect_warning(expect_null(plot(test, specific.args = list(legend = list("bottomright")))))


    # More complex example
    test <- test.metric(space, metric = c(sum, variances), steps = 5,
                        shifts = c("random", "size", "density", "position"), verbose = FALSE)
    expect_is(test, c("dispRity", "test.metric"))
    expect_equal(names(test), c("call", "results", "models", "saved_steps"))
    expect_equal(names(test$results), c("random", "size.increase", "size.hollowness", "density.higher", "density.lower", "position.top", "position.bottom"))
    expect_is(test$results[[1]], "data.frame")
    expect_is(test$models[[1]], "lm")

    ## Print works
    print_out <- capture.output(test)
    expect_equal(print_out, 
        c("Metric testing:",
          "The following metric was tested: c(sum, variances).",
          "The test was run on the random, size, density, position shifts for 3 replicates using the following model:",
          "lm(disparity ~ reduction, data = data)",
          "Use summary(x) or plot(x) for more details."))#,
          # "Use summary(item) or plot(item) for more details." ,
          # "Use summary(value) or plot(value) for more details."))


    ## Summarising basic works
    expect_equal(dim(summary(test)), c(7, 8))
    expect_is(summary(test), "matrix")
    expect_equal(rownames(summary(test)), names(test$results))
    expect_equal(colnames(summary(test)), c("10%", "32.5%", "55%", "77.5%", "100%", "slope", "p_value", "R^2(adj)"))

    ## Plot works
    expect_null(plot(test))
    expect_null(plot(test, specific.args = list(legend = list(list(pch = 2), list(pch = 19)))))

    ## Applying the test directly on a disparity object
    test <- test.metric(disparity, shifts = "evenness", verbose = FALSE)
    expect_is(test, c("dispRity", "test.metric"))
    expect_equal(names(test), c("call", "results", "models", "saved_steps"))
    expect_equal(names(test$results), c("evenness.flattened", "evenness.compacted"))
    expect_is(test$results[[1]], "data.frame")
    expect_is(test$models[[1]], "lm")

    ## Print works
    print_out <- capture.output(test)
    expect_equal(print_out, 
        c("Metric testing:",
          "The following metric was tested: c(median, centroids).",
          "The test was run on the evenness shift for 3 replicates using the following model:",
          "lm(disparity ~ reduction, data = data)",
          "Use summary(x) or plot(x) for more details."))#,
          # "Use summary(item) or plot(item) for more details." ,
          # "Use summary(value) or plot(value) for more details."))

    ## Summarising basic works
    expect_equal(dim(summary(test)), c(2, 13))
    expect_is(summary(test), "matrix")
    expect_equal(rownames(summary(test)), names(test$results))
    expect_equal(colnames(summary(test)), c(paste0(seq(from = 10, to = 100, by = 10), "%"), "slope", "p_value", "R^2(adj)"))

    ## Plot works
    expect_null(plot(test))

    ## Saving the results for visualisation
    space <- space.maker(300, 2, runif)
    
    ## Re-running the test on two shifts with data saving for visualisation
    test <- test.metric(space, metric = c(sum, variances), shifts = c("random", "size"), save.steps = TRUE)
    expect_is(test$saved_steps[[1]], "dispRity")

    ## Visualising the tests results and display the shifts visualisation
    expect_null(plot(test))

    error <- capture_error(plot(test, specific.args = list(visualise.steps = c(1, 11, 12))))
    expect_equal(error[[1]], "Impossible to display the steps 11, 12 because the test only contains 10 steps.")
    test <- test.metric(space, metric = c(sum, variances), shifts = c("density", "size"), save.steps = TRUE)
    expect_null(plot(test))

    ## Visualising the tests results with several specific options
    expect_null(plot(test,
         specific.args = list(visualise.steps = c(1,4,5))))

})