context("sequential.test")

#Testing data
data(BeckLee_mat50)
factors <- as.data.frame(matrix(data = c(rep(1, 12), rep(2, 13), rep(3, 25)),
     dimnames = list(rownames(BeckLee_mat50))), ncol = 1)
sum_of_variances <- dispRity(boot.matrix(cust.series(BeckLee_mat50, factors), bootstraps = 100), metric = c(sum, variances))
series <- extract.dispRity(sum_of_variances, observed = FALSE)
seq_series <- list(c(1,2), c(2,3))

test_that("set.pair.series works", {
    #Errors
    expect_error(
        set.pair.series(series["a"], intercept = NULL)
        )
    expect_error(
        set.pair.series("a", intercept = NULL)
        )
    #Normal results
    expect_is(
        set.pair.series(series[seq_series[[1]]], intercept = NULL)
        , "data.frame")
    expect_equal(
        dim(set.pair.series(series[seq_series[[1]]], intercept = NULL))
        , c(200, 2))
    expect_equal(
        dim(set.pair.series(series[seq_series[[1]]], intercept = "a"))
        , c(200, 3))
})

test_that("intercept.estimate works", {
    #Errors
    expect_error(
        intercept.estimate(intercept0 = "a", slopes = 2)
        )
    expect_error(
        intercept.estimate(intercept0 = 1, slopes = "a")
        )
    #Normal results
    expect_is(
        intercept.estimate(intercept0 = 1, slopes = 2)
        , "numeric")
    expect_equal(
        intercept.estimate(intercept0 = 2, slopes = 3)
        , 2 + 3 * 1)
    expect_equal(
        intercept.estimate(intercept0 = 2, slopes = c(3,3))
        , 8)
})

test_that("create.model works", {
    #Errors
    expect_error(
        create.model("a", family = gaussian)
        )
    expect_error(
        create.model(matrix(2,2), family = gaussian)
        )
    expect_error(
        create.model(set.pair.series(series[seq_series[[1]]]), family = "whatever")
        )
    #Normal results
    expect_is(
        create.model(set.pair.series(series[seq_series[[1]]]), family = gaussian)
        , c("glm","lm"))
    expect_equal(
        length(create.model(set.pair.series(series[seq_series[[1]]]), family = gaussian))
        , 30)
})

test_that("save.results works", {
    model <- create.model(set.pair.series(series[seq_series[[1]]]), family = gaussian)
    #Errors
    expect_true(
        is.na(save.results(model = "model", results = "coefficients"))
        )
    expect_true(
        is.null(save.results(model = model, results = "whatever")[[1]])
        )
    #Normal results
    expect_is(
        save.results(model = model, results = "coefficients")
        , "list")
    expect_equal(
        length(save.results(model = model, results = "coefficients"))
        , 1)
    expect_equal(
        length(save.results(model = model, results = c("coefficients", "terms", "family")))
        , 3)
    expect_equal(
        names(save.results(model = model, results = c("coefficients", "terms", "family")))
        , c("coefficients", "terms", "family"))
})

test_that("sequential.test works", {
    #Errors
    expect_error(
        sequential.test(sum_of_variances, family = gaussian)
        )
    expect_error(
        sequential.test(series, family = c(1, 2))
        )
    #results
    expect_is(
        sequential.test(series, family = gaussian)
        , "list")
    expect_equal(
        length(sequential.test(series, family = gaussian))
        , 2)
    expect_equal(
        names(sequential.test(series, family = gaussian))
        , c("Intercept", "Slope"))
})