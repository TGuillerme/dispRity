#TESTING null.test

context("model.test")

## Select model data
data(BeckLee_mat99) ; data(BeckLee_ages) ; data(BeckLee_tree)
data_bootstrapped <- boot.matrix(time.subsamples(BeckLee_mat99, BeckLee_tree, method = "continuous", rev(seq(from = 0, to = 120, by = 5)), model = "gradual"))
data <- dispRity(data_bootstrapped, c(sum, variances))

test_that("select.model.list internal", {
    model_input <- select.model.list(data)

    expect_is(model_input, "list")
    expect_equal(names(model_input), c("central_tendency", "variance", "sample_size", "subsamples"))
    expect_is(unique(unlist(lapply(model_input, class))), "numeric")

    ## MISSING OPTIONAL ARGUMENTS TEST!
})


## get.models.names
match_call <- model.test(1, models = c(mean, c(mean, median), c(mean, mean, mean)))

test_that("get.models.names internal", {
    expect_equal(get.models.names(match_call, time.split = NULL), c("mean", "mean:median", "multimean"))
    expect_equal(get.models.names(match_call, time.split = list(NULL, 88)), c("mean", "mean:median88", "multimean"))
    expect_equal(get.models.names(match_call, time.split = 88), c("mean", "mean:median88", "multimean"))
})

