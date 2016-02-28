#TESTING null.test

context("null.test")

# Testing data
data(BeckLee_mat50)
single_disp <- dispRity(BeckLee_mat50, metric = ellipse.volume)
factors <- as.data.frame(matrix(data = c(rep(1, nrow(BeckLee_mat50)/2),
     rep(2, nrow(BeckLee_mat50)/2)), nrow = nrow(BeckLee_mat50), ncol = 1,
     dimnames = list(rownames(BeckLee_mat50))))
multi_disp <- dispRity(boot.matrix(cust.series(BeckLee_mat50, factors), bootstraps = 100), metric = c(sum, variances))


#get.from.call
test_that("get.from.call works", {
    #Errors
    expect_error(
        get.from.call(data = "a", what = "metric", eval = TRUE)
        )
    expect_error(
        get.from.call(data = single_disp, what = "nothing", eval = TRUE)
        )
    #Right outputs
    expect_is(
        get.from.call(data = single_disp, what = "metric", eval = TRUE)
        , "function")
    expect_is(
        get.from.call(data = single_disp, what = "metric", eval = FALSE)
        , "character")
    expect_is(
        get.from.call(data = multi_disp, what = "metric", eval = TRUE)
        , "list")
    expect_is(
        get.from.call(data = multi_disp, what = "metric", eval = FALSE)
        , "character")
    expect_equal(
        get.from.call(data = single_disp, what = "dimensions", eval = TRUE)
        , 48)
    expect_equal(
        get.from.call(data = single_disp, what = "dimensions", eval = FALSE)
        , "48")
})

#make.null.model
test_that("make.null.model works", {
    #Errors
    expect_error(
        make.null.model("a", replicates = 5, null.distrib = rnorm, null.args = NULL, scale = FALSE)
        )
    expect_error(
        make.null.model(single_disp, replicates = -3, null.distrib = rnorm, null.args = NULL, scale = FALSE)
        )
    expect_error(
        make.null.model(single_disp, replicates = 5, null.distrib = "rnorm", null.args = NULL, scale = FALSE)
        )
    expect_error(
        make.null.model(single_disp, replicates = 5, null.distrib = rnorm, null.args = TRUE, scale = FALSE)
        )

    #Right output
    expect_is(
        make.null.model(single_disp, replicates = 5, null.distrib = rnorm, null.args = NULL, scale = FALSE)
        , "numeric")
    expect_equal(
        length(make.null.model(single_disp, replicates = 5, null.distrib = rnorm, null.args = NULL, scale = FALSE))
        , 5)

    #Handling args properly
    my_distributions1 <- unlist(replicate(16, list(rnorm, runif, rnorm), simplify = FALSE), recursive = FALSE)
    my_distributions2 <- unlist(replicate(16, list(rnorm, runif, rgamma), simplify = FALSE), recursive = FALSE)
    set.seed(1)
    my_args <- unlist(replicate(16, list(list(mean = runif(1), sd = runif(1)), list(min = 0.1, max = 0.8), list(shape = runif(1))), simplify = FALSE), recursive = FALSE)
    expect_is(
        make.null.model(single_disp, replicates = 5, null.distrib = my_distributions1, null.args = NULL, scale = FALSE)
        , "numeric")
    expect_is(
        make.null.model(single_disp, replicates = 5, null.distrib = my_distributions2, null.args = my_args, scale = FALSE)
        , "numeric")
})

#null.test
test_that("null.test works", {
    #Errors
    expect_error(
        null.test("a", replicates = 10, null.distrib = rnorm, null.args = NULL, alter = "two-sided", scale = FALSE)
        )
    expect_error(
        null.test(single_disp, replicates = "10", null.distrib = NULL, null.args = NULL, alter = "two-sided", scale = FALSE)
        )
    expect_error(
        null.test(single_disp, replicates = 10, null.distrib = rnorm, null.args = "NULL", alter = "two-sided", scale = FALSE)
        )
    expect_error(
        null.test(single_disp, replicates = 10, null.distrib = rnorm, null.args = NULL, alter = "something", scale = FALSE)
        )
    #Right output
    expect_is(
        null.test(single_disp, replicates = 10, null.distrib = rnorm, null.args = NULL, alter = "two-sided", scale = FALSE)
        , "randtest")
    expect_is(
        null.test(multi_disp, replicates = 10, null.distrib = rnorm, null.args = NULL, alter = "two-sided", scale = FALSE)
        , "list")
    expect_equal(
        unique(unlist(lapply(null.test(multi_disp, replicates = 10, null.distrib = rnorm, null.args = NULL, alter = "two-sided", scale = FALSE), class)))
        , "randtest")
})