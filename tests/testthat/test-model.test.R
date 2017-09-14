#TESTING null.test

context("model.test")

## Select model data
load("model_test_data.Rda")
data <- model_test_data

test_that("select.model.list internal", {
    model_input <- select.model.list(data)

    expect_is(model_input, "list")
    expect_equal(names(model_input), c("central_tendency", "variance", "sample_size", "subsamples"))
    expect_equal(unique(unlist(lapply(model_input, class))), "numeric")

    ## MISSING OPTIONAL ARGUMENTS TEST!
})


## get.models.names
get.call <- function(data, models, ...) {
    match_call <- match.call()
    return(match_call)
}

match_call <- get.call(1, models = list(mean, c(mean, median), c(mean, mean, mean)))

test_that("get.models.names internal", {
    expect_equal(
        get.models.names(match_call, time.shifts = NULL)
        , c("mean", "mean:median", "multimean"))
    expect_equal(
        get.models.names(match_call, time.shifts = list(NULL, 88))
        , c("mean", "mean:median88", "multimean"))
    expect_equal(
        get.models.names(match_call, time.shifts = 88)
        , c("mean", "mean:median88", "multimean"))
})

## check.shift.length
subsamples <- seq(1:10)

test_that("check.shift.length internal", {
    expect_is(check.shift.length(5, subsamples), "list")
    expect_equal(
        check.shift.length(5, subsamples)
        , list(c(5,5)))

    expect_equal(
        check.shift.length(c(2,8), subsamples)
        , list(c(2,8), c(8,2)))
})

## lapply.model.test
# subsamples <- seq(1:10)

# test_that("check.shift.length internal", {
#     expect_is(check.shift.length(5, subsamples), "list")
#     expect_equal(
#         check.shift.length(5, subsamples)
#         , list(c(5,5)))

#     expect_equal(
#         check.shift.length(c(2,8), subsamples)
#         , list(c(2,8), c(8,2)))
# })