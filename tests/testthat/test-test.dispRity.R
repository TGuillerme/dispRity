#TESTING boot.matrix

context("test.dispRity")

#Testing internal fun
test_that("flip.ref.lapply internal fun", {
    expect_error(
    	flip.ref.lapply(1, 2, t.test)
    	)
    expect_is(
    	flip.ref.lapply(rnorm(15), rnorm(15), t.test), "htest"
    	)
    expect_is(
    	lapply(replicate(3,rnorm(15), simplify=F), flip.ref.lapply, referential=rnorm(15), test=t.test), "list"
    	)
    expect_equal(
    	length(lapply(replicate(3,rnorm(15), simplify=F), flip.ref.lapply, referential=rnorm(15), test=t.test)), 3
    	)
})

test_that("test.list.lapply internal fun", {
    expect_error(
    	test.list.lapply(1, 2, t.test)
    	)
    expect_is(
    	test.list.lapply(c(1,2), replicate(2,rnorm(15), simplify=F), t.test), "htest"
    	)
    expect_is(
    	lapply(list(c(1,2), c(1,3)), test.list.lapply, replicate(3,rnorm(15), simplify=F), t.test), "list"
    	)
    expect_equal(
    	length(lapply(list(c(1,2), c(1,3)), test.list.lapply, replicate(3,rnorm(15), simplify=F), t.test)), 2
    	)
})

test_that("set.sequence internal fun", {
    expect_error(
    	set.sequence("a")
    	)
    expect_is(
    	set.sequence(4), "matrix"
    	)
    expect_equal(
    	dim(set.sequence(4)), c(2,3)
    	)
    expect_equal(
    	min(set.sequence(4)), 1
    	)
    expect_equal(
    	max(set.sequence(4)), 4
    	)
})

test_that("convert.to.numeric internal fun", {
    expect_true(
    	is.na(unlist(convert.to.numeric("a", "b")))
    	)
    expect_is(
    	convert.to.numeric(list(c("a", "b"), c("b", "a")), list("a"=1, "b"=2)), "list"
    	)
    expect_equal(
    	length(convert.to.numeric(list(c("a", "b"), c("b", "a")), list("a"=1, "b"=2))), 2
    	)
    expect_equal(
    	unlist(convert.to.numeric(list(c("a", "b"), c("b", "a")), list("a"=1, "b"=2))), c(1,2,2,1)
    	)
})

test_that("convert.to.character internal fun", {
    expect_true(
    	is.null(unlist(convert.to.character(1,1)))
    	)
    expect_is(
    	convert.to.character(list(c(1,2), c(2,1)), list("a"=rnorm(5),"b"=rnorm(5))), "list"
    	)
    expect_equal(
    	length(convert.to.character(list(c(1,2), c(2,1)), list("a"=rnorm(5),"b"=rnorm(5)))), 2
    	)
    expect_equal(
    	unlist(convert.to.character(list(c(1,2), c(2,1)), list("a"=rnorm(5),"b"=rnorm(5)))), c("a", "b", "b", "a")
    	)
})

test_that("list.to.table internal fun", {
    expect_error(
    	is.na(list.to.table(1))
    	)
    expect_is(
    	list.to.table(list("a"=rnorm(5),"b"=rnorm(5),"c"=rnorm(5))), "data.frame"
    	)
    expect_equal(
    	dim(list.to.table(list("a"=rnorm(5),"b"=rnorm(5),"c"=rnorm(5)))), c(15,2)
    	)
    expect_equal(
    	unique(list.to.table(list("a"=rnorm(5),"b"=rnorm(5),"c"=rnorm(5)))[,2]), as.factor(c("a","b","c"))
    	)
})

test_that("htest.to.vector internal fun", {
    expect_error(
    	htest.to.vector(t.test(rnorm(10), rnorm(10)), print="NA")
    	)
    set.seed(1)
    expect_equal(
    	htest.to.vector(t.test(rnorm(10), rnorm(10)), print="p.value"), 0.7840382
    	)
    set.seed(1)
    expect_equal(
    	htest.to.vector(t.test(rnorm(10), rnorm(10)), print=list("p.value")), 0.7840382
    	)
    set.seed(1)
    test <- htest.to.vector(t.test(rnorm(10), rnorm(10)), print=list("parameter", "p.value", "statistic"))
    expect_is(
    	test, "numeric"
    	)
    expect_equal(
    	as.vector(test), c(16.4689565,0.7840382,-0.2785755)
    	)
})

test_that("example works fine", {
    set.seed(1)
    ## Load the Beck & Lee 2014 data
    data(BeckLee_mat50)

    ## Calculating the disparity from a customised series
    ## Generating the series
    factors <- as.data.frame(matrix(data = c(rep(1, 12), rep(2, 13), rep(3, 25)), dimnames =list(rownames(BeckLee_mat50))), ncol = 1)
    customised_series <- cust.series(BeckLee_mat50, factors)
    ## Bootstrapping the data
    bootstrapped_data <- boot.matrix(customised_series, bootstraps=100)
    ## Caculating the sum of ranges
    sum_of_ranges <- dispRity(bootstrapped_data, metric=c(sum, ranges))

    ## Measuring the series overlap
    expect_is(
    	test.dispRity(sum_of_ranges, bhatt.coeff, "pairwise"), "data.frame"
    	)
    expect_equal(
    	dim(test.dispRity(sum_of_ranges, bhatt.coeff, "pairwise")), c(3,1)
    	)
    expect_equal(
    	sum(test.dispRity(sum_of_ranges, bhatt.coeff, "pairwise")), 0.67027
    	)

    ## Measuring differences from a reference_series
    expect_is(
    	test.dispRity(sum_of_ranges, wilcox.test, "referential"), "data.frame"
    	)
    expect_equal(
    	dim(test.dispRity(sum_of_ranges, wilcox.test, "referential")), c(2,2)
    	)
    expect_equal(
    	sum(test.dispRity(sum_of_ranges, wilcox.test, "referential")[,1]), 1543
    	)
    expect_equal(
    	sum(test.dispRity(sum_of_ranges, wilcox.test, "referential")[,2]), 3.025477e-17
    	)
    
    ## Testing the effect of the factors
    expect_is(
    	test.dispRity(sum_of_ranges, aov, "all"), c("aov", "lm")
    	)
    expect_equal(
    	test.dispRity(sum_of_ranges, aov, "all")$rank, 3
    	)
    expect_equal(
    	as.vector(test.dispRity(sum_of_ranges, aov, "all")$coefficients), c(24.048441, 2.053728, 9.798655)
    	)
})
