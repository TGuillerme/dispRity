#TESTING dispRity

context("get.dispRity")

#Loading the data
load("test_data.Rda"
    	)
data<-test_data$ord_data_tips_nodes
tree<-test_data$tree

#Make a time series example
suppressMessages(series <- time.series(data, tree, method="c", time=5, model="acctran"))
#Make a bootstrap example
bootstraps <- boot.matrix(series, bootstrap=10, rarefaction=TRUE)
#Make a disparity example
disparity <- dispRity(bootstraps, metric=c(sum, ranges))

#Sanitizing
test_that("Sanitizing works", {
    expect_error(
    	get.dispRity(data="a", what=NULL)
    	)
    expect_error(
    	get.dispRity(data=disparity, what=NULL)
    	)
    expect_error(
    	get.dispRity(data=disparity, what='a')
    	)
    expect_error(
    	get.dispRity(data=disparity, what=c(1:6))
    	)
    expect_error(
    	get.dispRity(data=disparity, what=c(1:3), keep.elements="yes")
    	)
})

#Testing with series objects
test_that("get.dispRity works with a series object", {
    expect_is(
    	get.dispRity(data=series), "dispRity"
    	)
    expect_is(
    	get.dispRity(data=series, what=c(1,2)), "dispRity"
    	)
    expect_is(
    	get.dispRity(data=series, what="100.13328", keep.elements=TRUE), "dispRity"
    	)
    expect_equal(
    	get.dispRity(data=series), series
    	)
    expect_equal(
    	length(get.dispRity(data=series, what=c(1,2))$series), 3
        ) #one additional series (series type
    expect_equal(
    	length(get.dispRity(data=series, what="100.13328")$series), 2
        ) #one additional series (series type
    expect_equal(
    	length(get.dispRity(data=series, what="100.13328", keep.elements=TRUE)$elements), 99
    	)
})

#Testing with bootstraps objects
test_that("get.dispRity works with a bootstraps object", {
    expect_is(
    	get.dispRity(data=bootstraps), "dispRity"
    	)
    expect_is(
    	get.dispRity(data=bootstraps, what=c(1,2)), "dispRity"
    	)
    expect_is(
    	get.dispRity(data=bootstraps, what="100.13328", keep.elements=TRUE), "dispRity"
    	)
    expect_equal(
    	get.dispRity(data=bootstraps), bootstraps
    	)
    expect_equal(
    	length(get.dispRity(data=bootstraps, what=c(1,2))$series), 2
    	)
    expect_equal(
    	length(get.dispRity(data=bootstraps, what="100.13328")$series), 1
    	)
    expect_equal(
    	length(get.dispRity(data=bootstraps, what="100.13328", keep.elements=TRUE)$elements), 99
    	)
})

#Testing with disparity objects
test_that("get.dispRity works with a disparity object", {
    expect_is(
    	get.dispRity(data=disparity), "dispRity"
    	)
    expect_is(
    	get.dispRity(data=disparity, what=c(1,2)), "dispRity"
    	)
    expect_is(
    	get.dispRity(data=disparity, what="100.13328", keep.elements=TRUE), "dispRity"
    	)
    expect_equal(
    	get.dispRity(data=disparity), disparity
    	)
    expect_equal(
    	length(get.dispRity(data=disparity, what=c(1,2))$series), 2
    	)
    expect_equal(
    	length(get.dispRity(data=disparity, what="100.13328")$series), 1
    	)
    expect_equal(
    	length(get.dispRity(data=disparity, what="100.13328", keep.elements=TRUE)$elements), 99
    	)
})
