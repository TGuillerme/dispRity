#TESTING make.metric

context("make.metric")

#Loading the data
#load("test_data.Rda")
#data<-test_data$ord_data_tips

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
})