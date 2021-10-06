## Testing the mini chains pipeline
load("covar_model_list.Rda")
load("covar_char_data.Rda")
load("covar_tree_data.Rda")


## Test
test_that("get.covar works", {
    ## Works on a dispRity object
    data_test <- MCMCglmm.subsets(data = covar_char_data, posteriors = covar_model_list[[1]])

    ## works on a MCMCglmm object (basically calls MCMCglmm.covars)
    test <- get.covar(data_test)
    expect_equal(length(test), length(MCMCglmm.levels(covar_model_list[[1]])))
    expect_equal(length(test[[1]]), 1000)
    ## n argument works
    test <- get.covar(data_test, n = 7)
    expect_equal(length(test), length(MCMCglmm.levels(covar_model_list[[1]])))
    expect_equal(length(test[[1]]), 7)
    ## sample argument works
    test <- get.covar(data_test, sample = c(42, 5))
    expect_equal(length(test), length(MCMCglmm.levels(covar_model_list[[1]])))
    expect_equal(length(test[[1]]), 2)
    ## sample argument works as a function
    test <- get.covar(data_test, sample = median)
    expect_equal(length(test), length(MCMCglmm.levels(covar_model_list[[1]])))
    expect_equal(length(test[[1]]), 1)
    ## Only takes the n argument
    warn <- capture_warnings(test <- get.covar(data_test, sample = 42, n = 7))
    expect_equal(warn[[1]], "sample argument is ignored since n = 7 random samples are asked for.")
    expect_equal(length(test), length(get.covar(data_test)))
    expect_equal(length(test[[1]]), 7)
    
    ## Subsets argument works
    data_test <- MCMCglmm.subsets(data = covar_char_data, posteriors = covar_model_list[[7]])
    test <- get.covar(data_test, sample = 1, subsets = c(2,1))
    expect_equal(length(test), 2)
    expect_equal(names(test), c("animal:clade_2", "animal:clade_1"))
    expect_equal(length(test[[1]]), 1)
    
})

## Test
test_that("get.one.axis works", {
    set.seed(1)
    beer <- MCMCglmm.covars(covar_model_list[[7]], n = 1)
    ## Right output
    expect_is(get.one.axis(beer[[1]][[1]]), "matrix")
    expect_equal(dim(get.one.axis(beer[[1]][[1]])), c(2,3))
    ## Selects the right axes
    test1 <- get.one.axis(beer[[1]][[1]], axis = 1)
    test2 <- get.one.axis(beer[[1]][[1]], axis = 2)
    test3 <- get.one.axis(beer[[1]][[1]], axis = 3)
    expect_true(all(test1 != test2))
    expect_true(all(test1 != test3))
    expect_true(all(test2 != test3))
    ## Levels works
    test2 <- get.one.axis(beer[[1]][[1]], level = 1)
    expect_true(all(is.nan(test2)))
    test2 <- get.one.axis(beer[[1]][[1]], level = 0.5)
    expect_true(all(test1 != test2))
    ## Dimensions selection works
    expect_equal(dim(get.one.axis(beer[[1]][[1]], dimensions = c(1,2))), c(2,2))
})

test_that("axis.covar works", {

    ## Works with normal models
    data <- MCMCglmm.subsets(data = covar_char_data, posteriors = covar_model_list[[7]])
    ## Get all the axis
    test <- axis.covar(data, n = 7)
    ## Right output
    expect_equal(names(test), unname(MCMCglmm.levels(covar_model_list[[7]])))
    expect_equal(length(test[[1]]), 7)
    expect_equal(unique(unlist(lapply(test[[1]], dim))), c(2, 3))
    
    ## Get the mean
    test <- axis.covar(data, sample = mean)
    ## Right output
    expect_equal(names(test), unname(MCMCglmm.levels(covar_model_list[[7]])))
    expect_equal(length(test[[1]]), 1)
    expect_equal(unique(unlist(lapply(test[[1]], dim))), c(2, 3))
})
