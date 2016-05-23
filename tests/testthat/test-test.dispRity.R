#TESTING boot.matrix

context("test.dispRity")

test_that("test.list.lapply.distributions internal fun", {
    my_list_of_comp <- list(c(1,2), c(2,1))
    my_data <- list(list(rnorm(10), rnorm(10)), list(rnorm(10), rnorm(10)))
    my_test <- t.test

    #Errors
    expect_error(
        test.list.lapply.distributions(1, my_data, my_test)
        )
    expect_error(
        test.list.lapply.distributions(my_list_of_comp, 1, my_test)
        )
    expect_error(
        test.list.lapply.distributions(my_list_of_comp, my_data, 1)
        )

    #Right output
    expect_is(
        test.list.lapply.distributions(my_list_of_comp, my_data, my_test)
        , "list")
    expect_equal(
        length(test.list.lapply.distributions(my_list_of_comp, my_data, my_test))
        ,2)
    expect_equal(
        length(test.list.lapply.distributions(my_list_of_comp, my_data, my_test)[[1]])
        ,2)
    expect_equal(
        unique(unlist(lapply(test.list.lapply.distributions(my_list_of_comp, my_data, my_test), lapply, class)))
        ,"htest")
})

test_that("set.sequence internal fun", {
    expect_error(
    	set.sequence("a")
    	)
    expect_is(
    	set.sequence(4)
        , "matrix")
    expect_equal(
    	dim(set.sequence(4))
        , c(2,3))
    expect_equal(
    	min(set.sequence(4))
        , 1)
    expect_equal(
    	max(set.sequence(4))
        , 4)
})

test_that("convert.to.numeric internal fun", {
    expect_true(
    	is.na(unlist(convert.to.numeric("a", "b")))
    	)
    expect_is(
    	convert.to.numeric(list(c("a", "b"), c("b", "a")), list("a"=1, "b"=2))
        , "list")
    expect_equal(
    	length(convert.to.numeric(list(c("a", "b"), c("b", "a")), list("a"=1, "b"=2)))
        , 2)
    expect_equal(
    	unlist(convert.to.numeric(list(c("a", "b"), c("b", "a")), list("a"=1, "b"=2)))
        , c(1,2,2,1))
})

test_that("convert.to.character internal fun", {
    expect_true(
    	is.null(unlist(convert.to.character(1,1)))
    	)
    expect_is(
    	convert.to.character(list(c(1,2), c(2,1)), list("a"=rnorm(5),"b"=rnorm(5)))
        , "list")
    expect_equal(
    	length(convert.to.character(list(c(1,2), c(2,1)), list("a"=rnorm(5),"b"=rnorm(5))))
        , 2)
    expect_equal(
    	unlist(convert.to.character(list(c(1,2), c(2,1)), list("a"=rnorm(5),"b"=rnorm(5))))
        , c("a", "b", "b", "a"))
})

test_that("list.to.table internal fun", {
    # expect_error(
    # 	is.na(list.to.table(1))
    # 	)
    expect_is(
    	list.to.table(list("a"=rnorm(5),"b"=rnorm(5),"c"=rnorm(5)))
        , "data.frame")
    expect_equal(
    	dim(list.to.table(list("a"=rnorm(5),"b"=rnorm(5),"c"=rnorm(5))))
        , c(15,2))
    expect_equal(
    	unique(list.to.table(list("a"=rnorm(5),"b"=rnorm(5),"c"=rnorm(5)))[,2])
        , as.factor(c("a","b","c")))
})

test_that("htest.to.vector internal fun", {
    expect_error(
    	htest.to.vector(t.test(rnorm(10), rnorm(10)), print="NA")
    	)
    set.seed(1)
    expect_equal(
    	htest.to.vector(t.test(rnorm(10), rnorm(10)), print="p.value")
        , 0.7840382)
    set.seed(1)
    expect_equal(
    	htest.to.vector(t.test(rnorm(10), rnorm(10)), print=list("p.value"))
        , 0.7840382)
    set.seed(1)
    test <- htest.to.vector(t.test(rnorm(10), rnorm(10)), print=list("parameter", "p.value", "statistic"))
    expect_is(
    	test
        , "numeric")
    expect_equal(
    	as.vector(test)
        , c(16.4689565,0.7840382,-0.2785755))
})


test_that("set.comparisons.list internal fun", {
    my_data <- list(list(rnorm(10), rnorm(10)), list(rnorm(10), rnorm(10)), list(rnorm(10), rnorm(10)))

    #Errors
    expect_error(
        set.comparisons.list("custom", my_data)
        )
    # expect_error(
    #     set.comparisons.list(my_data, "custom")
    #     )

    #Custom output
    expect_equal(
        set.comparisons.list("custom", 1, c(1,2,2,3))
        ,c(1,2,2,3))
    expect_equal(
        set.comparisons.list("custom", 1, TRUE)
        ,TRUE)
    #Pairwise output
    expect_equal(
        set.comparisons.list("pairwise", my_data, 1)
        ,list(c(1,2), c(1,3), c(2,3)))
    #Sequential output
    expect_equal(
        set.comparisons.list("sequential", my_data, 1)
        ,list(c(1,2), c(2,3)))
    #Referential output
    expect_equal(
        set.comparisons.list("referential", my_data, 1)
        ,list(c(1,2), c(1,3)))
})

test_that("save.comparison.list internal fun", {
    my_data <- list(list(rnorm(10), rnorm(10)), list(rnorm(10), rnorm(10)), list(rnorm(10), rnorm(10)))
    names(my_data) <- c("X", "Y")
    my_comp_series <- list(c(1,2), c(2,1))

    expect_equal(
        save.comparison.list(list(c(1,2), c(2,1)), my_data)
        ,c("X - Y", "Y - X"))
    expect_equal(
        save.comparison.list(list(c(1,2), c(1,2)), my_data)
        ,c("X - Y", "X - Y"))
    expect_equal(
        save.comparison.list(list(c(1,2)), my_data)
        ,c("X - Y"))
    expect_equal(
        save.comparison.list(1, my_data)
        ,c("X"))
})

test_that("get.quantiles.from.table internal fun", {
    my_table <- matrix(rnorm(50), nrow = 5)

    expect_error(
        get.quantiles.from.table(1, mean, c(0.45, 0.55))
        )
    expect_error(
        get.quantiles.from.table(my_table, 1, c(0.45, 0.55))
        )
    expect_error(
        get.quantiles.from.table(my_table, mean, 2)
        )

    expect_is(
        get.quantiles.from.table(my_table, mean, c(0.45, 0.55))
        ,"matrix")
    expect_equal(
        dim(get.quantiles.from.table(my_table, mean, c(0.45, 0.55)))
        ,c(5,3))
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
    	test.dispRity(sum_of_ranges, bhatt.coeff, "pairwise")
        , "data.frame")
    expect_equal(
    	dim(test.dispRity(sum_of_ranges, bhatt.coeff, "pairwise"))
        , c(3,1))
    expect_equal(
    	sum(test.dispRity(sum_of_ranges, bhatt.coeff, "pairwise"))
        , 0.67027)

    ## Measuring differences from a reference_series
    expect_is(
    	test.dispRity(sum_of_ranges, wilcox.test, "referential")
        , "list")
    expect_equal(
    	length(test.dispRity(sum_of_ranges, wilcox.test, "referential"))
        , 2)
    expect_equal(
    	sum(test.dispRity(sum_of_ranges, wilcox.test, "referential")[[1]][,1])
        , 1543)
    expect_equal(
    	sum(test.dispRity(sum_of_ranges, wilcox.test, "referential")[[2]][,1])
        , 3.025477e-17)

    ## Measuring disparity as a distribution
    disparity_var <- dispRity(bootstrapped_data, metric = variances)
    test1 <- test.dispRity(disparity_var, test = t.test, comparisons = "pairwise", concatenate = TRUE)
    expect_is(
        test1
        ,"list")
    expect_equal(
        unique(unlist(lapply(test1, class)))
        ,"data.frame")
    expect_equal(
        unique(unlist(lapply(test1, dim)))
        ,c(3,1))
    test2 <- test.dispRity(disparity_var, test = t.test, comparisons = "pairwise", concatenate = FALSE)
    expect_is(
        test2
        ,"list")
    expect_equal(
        unique(unlist(lapply(test2, class)))
        ,"matrix")
    expect_equal(
        unique(unlist(lapply(test2, dim)))
        ,c(3,5))

    ## Testing the effect of the factors
    expect_is(
        test.dispRity(sum_of_ranges, aov, "all")
        , c("aov", "lm"))
    expect_equal(
        test.dispRity(sum_of_ranges, aov, "all")$rank
        , 3)
    expect_equal(
        as.vector(test.dispRity(sum_of_ranges, aov, "all")$coefficients)
        , c(27.99772811, -0.03648708, 0.04100832))
})
