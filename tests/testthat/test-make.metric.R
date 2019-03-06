#TESTING make.metric

context("make.metric")

#Loading the data
#load("test_data.Rda")
#data<-test_data$ord_data_tips

test_that("check.metric works", {
    expect_error(check.metric(1))
    expect_equal(check.metric(sum), "summary.metric")
    expect_equal(check.metric(var), "class.metric")
})

test_that("Output is correct", {
    expect_error(
    	make.metric("a")
    	)
    expect_error(
    	make.metric(1)
    	)
    expect_error(
    	make.metric(function(x)as.character(x))
    	)

    ## Verbose
    test <- function(x) as.character(x)
    error <- capture_error(make.metric(test, verbose = TRUE))
    expect_equal(error[[1]],
        "The provided metric function generated an error or a warning!\nDoes the following work?\n    test(matrix(rnorm(20), 5,4))\nThe problem may also come from the optional arguments (...) in test."
        )

    error <- capture_error(make.metric(test, silent = FALSE))
    expect_equal(error[[1]],
        "The provided metric function generated an error or a warning!\nDoes the following work?\n    test(matrix(rnorm(20), 5,4))\nThe problem may also come from the optional arguments (...) in test."
        )

    expect_equal(
    	make.metric(mean, silent=TRUE), "level1"
    	)
    expect_equal(
    	make.metric(ranges, silent=TRUE), "level2"
    	)
    expect_equal(
    	make.metric(var, silent=TRUE), "level3"
    	)
    expect_equal(
    	make.metric(function(x)mean(var(x)), silent=TRUE), "level1"
    	)
    expect_equal(
    	make.metric(function(x)variances(var(x)), silent=TRUE), "level2"
    	)
    expect_equal(
    	make.metric(function(x)var(var(x)), silent=TRUE), "level3"
    	)
    expect_equal(
    	make.metric(function(x)sd(variances(var(x))), silent=TRUE), "level1"
    	)

    expect_equal(capture.output(make.metric(var)),
        c("var outputs a matrix object.", "var is detected as being a dimension-level 3 function.", "Additional dimension-level 2 and/or 1 function(s) will be needed."))

    expect_equal(capture.output(make.metric(sd)), 
        c("sd outputs a single value.", "sd is detected as being a dimension-level 1 function."))

    expect_equal(capture.output(make.metric(variances)), 
        c("variances outputs a matrix object.", "variances is detected as being a dimension-level 2 function."))

})