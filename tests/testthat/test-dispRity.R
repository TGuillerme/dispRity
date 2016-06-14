# TESTING dispRity

context("dispRity")


test_that("disparity.calc internal works", {
    set.seed(1)
    BSresult <- list(list(list(matrix(rnorm(20), 5, 4))))

    lvl3_result <- unlist(disparity.calc(BSresult[[1]], level3.fun = var, level2.fun = NULL, level1.fun = NULL))
    lvl3_expect <- as.vector(var(BSresult[[1]][[1]][[1]]))
    expect_equal(lvl3_result, lvl3_expect)

    lvl2_result <- unlist(disparity.calc(BSresult[[1]], level3.fun = NULL, level2.fun = variances, level1.fun = NULL))
    lvl2_expect <- as.vector(variances(BSresult[[1]][[1]][[1]]))
    expect_equal(lvl2_result, lvl2_expect)

    lvl1_result <- unlist(disparity.calc(BSresult[[1]], level3.fun = NULL, level2.fun = NULL, level1.fun = sd))
    lvl1_expect <- as.vector(sd(BSresult[[1]][[1]][[1]]))
    expect_equal(lvl1_result, lvl1_expect)
})

#Loading the data
load("test_data.Rda")
data<-test_data$ord_data_tips
factor<-as.data.frame(matrix(data=c(rep(1, nrow(data)/2),rep(2, nrow(data)/2)), nrow=nrow(data), ncol=1))
rownames(factor)<-rownames(data)

#Sanitizing
test_that("Sanitizing works", {
    expect_error(
    	dispRity(data="a", c(sum, ranges), FALSE)
    	)
    expect_error(
    	dispRity(data=1, c(sum, ranges), FALSE)
    	)
    expect_error(
    	dispRity(data, metric="a", FALSE)
    	)
    expect_error(
    	dispRity(data, metric=1, FALSE)
    	)
    expect_error(
    	dispRity(data, metric=c(1,2), FALSE)
    	)
    expect_error(
    	dispRity(data, metric=c(sum, data), FALSE)
    	)
    expect_error(
    	dispRity(data, c(sum, ranges), verbose="yes")
    	)
})
#Reset
test <- NULL ; data<-test_data$ord_data_tips

#Testing metric argument
test_that("metric argument works", {
    expect_error(
        dispRity(BeckLee_mat50, metric = var)
    )
    expect_error(
        dispRity(BeckLee_mat50, metric = c(var))
    )
    expect_is(
        dispRity(BeckLee_mat50, metric = c(var, sd))
        , "dispRity")
    expect_is(
        dispRity(BeckLee_mat50, metric = c(var, variances, sd))
        , "dispRity")
    expect_is(
        dispRity(BeckLee_mat50, metric = sd)
        , "dispRity")
    expect_is(
        dispRity(BeckLee_mat50, metric = c(sd))
        , "dispRity")
    expect_error(
        dispRity(BeckLee_mat50, metric = c(sd, sd))
        )
    expect_error(
        dispRity(BeckLee_mat50, metric = c(var, sd, sd))
        )
})

#one matrix
test<-dispRity(data, metric=c(sum, ranges))
test_that("dispRity works with a single matrix", {
    expect_is(
    	test, "dispRity"
    	)
    expect_equal(
    	names(test), c("data","disparity","elements","series","call")
    	)
    expect_is(
    	test$data$observed, "list"
    	)
    expect_equal(
    	length(test$series), 1
    	)
    expect_equal(
    	length(test$elements), 50
    	)
})
#Reset
test <- NULL ; data<-test_data$ord_data_tips

#bootstrapped
data<-boot.matrix(data, bootstrap=5, rarefaction=FALSE, boot.type="full")
test<-dispRity(data, metric=c(sum, ranges))
test_that("dispRity works with a bootstrapped matrix", {
    expect_is(
    	test, "dispRity"
    	)
    expect_equal(
    	names(test), c("data","disparity","elements","series","call")
    	)
    expect_is(
    	test$data$bootstraps, "list"
    	)
    expect_equal(
    	length(test$series), 1
    	)
    expect_equal(
    	length(test$elements), 50
    	)
})
#Reset
test <- NULL ; data<-test_data$ord_data_tips

#bootstrapped + rarefied
data<-test_data$ord_data_tips
data<-boot.matrix(data, bootstrap=5, rarefaction=TRUE, boot.type="full")
test<-dispRity(data, metric=c(sum, ranges))
test_that("dispRity works with a bootstrapped and rarefied matrix", {
    expect_is(
    	test, "dispRity"
    	)
    expect_equal(
    	names(test), c("data","disparity","elements","series","call")
    	)
    expect_is(
    	test$data$bootstraps, "list"
    	)
    expect_equal(
    	length(test$series), 1
    	)
    expect_equal(
    	length(test$elements), 50
    	)
})
#Reset
test <- NULL ; data<-test_data$ord_data_tips

#one matrix with series
data<-test_data$ord_data_tips
data<-cust.series(data, factor)
test<-dispRity(data, metric=c(sum, ranges))
test_that("dispRity works with custom series", {
    expect_is(
    	test, "dispRity"
    	)
    expect_equal(
    	names(test), c("data","disparity","elements","series","call")
    	)
    expect_is(
    	test$data$observed, "list"
    	)
    expect_equal(
    	length(test$series), 2
    	)
    expect_equal(
    	length(test$elements), 50
    	)
})
#Reset
test <- NULL ; data<-test_data$ord_data_tips

#bootstrapped + rarefied + series
factor<-as.data.frame(matrix(data=c(rep(1, nrow(data)/2),rep(2, nrow(data)/2)), nrow=nrow(data), ncol=1))
rownames(factor)<-rownames(data)
data<-cust.series(data, factor)
data<-boot.matrix(data, bootstrap=5, rarefaction=FALSE, boot.type="full")
test<-dispRity(data, metric=c(sum, ranges))
test_that("dispRity works with a bootstrapped, rarefied, custom series", {
    expect_is(
    	test, "dispRity"
    	)
    expect_equal(
    	names(test), c("data","disparity","elements","series","call")
    	)
    expect_is(
    	test$data$bootstraps, "list"
    	)
    expect_equal(
    	length(test$series), 2
    	)
    expect_equal(
    	length(test$elements), 50
    	)
})
#Reset
test <- NULL ; data<-test_data$ord_data_tips

#testing example
test_that("Example works", {
    data(BeckLee_mat50)
    sum_of_ranges <- dispRity(BeckLee_mat50, metric = c(sum, ranges))
    ex1<-summary(sum_of_ranges)
    expect_is(
    	ex1, "data.frame"
    	)
    expect_equal(
    	dim(ex1), c(1,3)
    	)

    bootstrapped_data <- boot.matrix(BeckLee_mat50, bootstraps=100)
    ex2<-dispRity(bootstrapped_data, metric=c(sum, ranges))
    expect_is(
    	ex2, "dispRity"
    	)
    expect_equal(
    	dim(ex2[[1]][[1]][[1]][[1]][[1]]), c(50,48)
    	)

    factors <- as.data.frame(matrix(data = c(rep(1, nrow(BeckLee_mat50)/2), rep(2, nrow(BeckLee_mat50)/2)), nrow = nrow(BeckLee_mat50), ncol = 1, dimnames = list(rownames(BeckLee_mat50))))
    customised_series <- cust.series(BeckLee_mat50, factors)
    set.seed(1)
    bootstrapped_data <- boot.matrix(customised_series, bootstraps=100)
    sum_of_ranges <- dispRity(bootstrapped_data, metric=c(sum, ranges))

    ex3 <- summary(sum_of_ranges, rounding = 2)

    expect_is(
    	ex3
        , "data.frame")
    expect_equal(
        dim(ex3)
        , c(2,8))
    expect_equal(
    	sum(ex3[,4])
        , sum(c(32.67,33.85)))
})
#Reset
test <- NULL ; data<-test_data$ord_data_tips

#testing with distributions as output (level2 functions outputs)
test_that("dispRity works with level2 functions", {
    data(BeckLee_mat50)
    ranges_test <- dispRity(BeckLee_mat50, metric = ranges)
    #Output is a distribution of ncol(data) ranges
    expect_equal(
        ncol(ranges_test$data[[1]][[1]]), length(ranges_test$disparity$observed[[1]][[1]][[1]])
        )
    #Output is a distribution of ncol(data) ranges (works for bootstraps as well)
    ranges_test <- dispRity(boot.matrix(BeckLee_mat50, 2), metric = ranges)
    expect_equal(
        ncol(ranges_test$data[[1]][[1]][[1]][[1]]), length(ranges_test$disparity$bootstrapped[[1]][[1]][[1]])
        )
    expect_equal(
        ncol(ranges_test$data[[1]][[1]][[1]][[1]]), length(ranges_test$disparity$bootstrapped[[1]][[1]][[2]])
        )
})

#testing with disparity inputs
test_that("dispRity works with disparity inputs", {
    ranges_test <- dispRity(data, metric = ranges)
    set.seed(1)
    ranges_test_bs <- dispRity(boot.matrix(data, 10), metric = ranges)
    ranges_test_series <- dispRity(cust.series(data, factor), metric = ranges)
    set.seed(1)
    ranges_test_series_bs <- dispRity(boot.matrix(cust.series(data, factor), 10), metric = ranges)

    mean_ranges_test <- dispRity(ranges_test, metric = mean)
    set.seed(1)
    mean_ranges_test_bs <- dispRity(ranges_test_bs, metric = mean)
    mean_ranges_test_series <- dispRity(ranges_test_series, metric = mean)
    set.seed(1)
    mean_ranges_test_series_bs <- dispRity(ranges_test_series_bs, metric = mean)

    #The calculated mean in mean_ranges_test must be equal to the mean in range_test summary
    expect_equal(
        summary(ranges_test)[1,3], summary(mean_ranges_test)[1,3]
        )
    expect_equal(
        summary(ranges_test_bs)[1,3], summary(mean_ranges_test_bs)[1,3]
        )
    expect_equal(
        summary(ranges_test_series)[c(1:2),3], summary(mean_ranges_test_series)[c(1:2),3]
        )
    expect_equal(
        summary(ranges_test_series_bs)[c(1:2),3], summary(mean_ranges_test_series_bs)[c(1:2),3]
        )

    #Same for the bs values
    expect_equal(
        summary(ranges_test_bs)[1,4], summary(mean_ranges_test_bs)[1,4]
        )
    expect_equal(
        summary(ranges_test_series_bs)[c(1:2),4], summary(mean_ranges_test_series_bs)[c(1:2),4]
        )
})