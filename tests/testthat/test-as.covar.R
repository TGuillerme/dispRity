## Test

test_that("as.covar works in standalone", {
    ## Creating a dispRity
    data(charadriiformes)
    covar_data <- MCMCglmm.subsets(data       = charadriiformes$data,
                                   posteriors = charadriiformes$posteriors)    

    ## Testing the handling
    match_call <- list()

    ## level 3 covar
    metric <- as.covar(var)
    expect_true(check.covar(metric, covar_data)$is_covar)
    test <- get.dispRity.metric.handle(c(sum, metric), match_call, data = covar_data, tree = NULL)$levels 
    expect_true(!is.null(test$level3.fun))
    expect_true(is.null(test$level2.fun))
    expect_true(!is.null(test$level1.fun))
    expect_true(eval.covar(test$level3.fun, null.return = FALSE))
    expect_false(eval.covar(test$level1.fun, null.return = FALSE))

    ## level 2 covar
    metric <- as.covar(variances)
    expect_true(check.covar(metric, covar_data)$is_covar)
    test <- get.dispRity.metric.handle(metric, match_call, data = covar_data, tree = NULL)$levels
    expect_true(is.null(test$level3.fun))
    expect_true(!is.null(test$level2.fun))
    expect_true(is.null(test$level1.fun))
    expect_true(eval.covar(test$level2.fun, null.return = FALSE))

    ## level 1 covar (with no formals)
    metric <- as.covar(sum)
    expect_true(check.covar(metric, covar_data)$is_covar)
    test <- get.dispRity.metric.handle(metric, match_call, data = covar_data, tree = NULL)$levels
    expect_true(is.null(test$level3.fun))
    expect_true(is.null(test$level2.fun))
    expect_true(!is.null(test$level1.fun))
    expect_true(eval.covar(test$level1.fun, null.return = FALSE))

    ## level 1 covar (with formals)
    metric <- as.covar(ellipse.volume)
    expect_true(check.covar(metric, covar_data)$is_covar)
    test <- get.dispRity.metric.handle(metric, match_call, data = covar_data, tree = NULL)$levels
    expect_true(is.null(test$level3.fun))
    expect_true(is.null(test$level2.fun))
    expect_true(!is.null(test$level1.fun))
    expect_true(eval.covar(test$level1.fun, null.return = FALSE))

    ## pairs of metrics:
    # Possible combinations:
    # covar(lvl1)
    # lvl1 + covar(lvl2)
    # lvl1 + lvl2 + covar(lvl3)

    metric <- c(sum, as.covar(variances))
    test <- get.dispRity.metric.handle(metric, match_call, data = covar_data, tree = NULL)$levels
    expect_true(is.null(test$level3.fun))
    expect_true(!is.null(test$level2.fun))
    expect_true(!is.null(test$level1.fun))
    expect_true(eval.covar(test$level2.fun, null.return = FALSE))
    expect_false(eval.covar(test$level1.fun, null.return = FALSE))

    metric <- c(sd, variances, as.covar(var))
    test <- get.dispRity.metric.handle(metric, match_call, data = covar_data, tree = NULL)$levels
    expect_true(!is.null(test$level3.fun))
    expect_true(!is.null(test$level2.fun))
    expect_true(!is.null(test$level1.fun))
    expect_true(eval.covar(test$level3.fun, null.return = FALSE))
    expect_false(eval.covar(test$level2.fun, null.return = FALSE))
    expect_false(eval.covar(test$level1.fun, null.return = FALSE))

    metric <- c(as.covar(sum), variances)
    error <- capture_error(get.dispRity.metric.handle(metric, match_call, data = covar_data, tree = NULL))
    expect_equal(error[[1]], "Only the highest dimension-level metric can be set as as.covar().")
    metric <- c(as.covar(sum), as.covar(variances))
    error <- capture_error(get.dispRity.metric.handle(metric, match_call, data = covar_data, tree = NULL))
    expect_equal(error[[1]], "Only one metric can be set as as.covar().")
})

test_that("as.covar works in dispRity", {
    # data(charadriiformes)

    # ## Test works OK with base
    # data0 <- custom.subsets(data  = charadriiformes$data[, -c(18, 19)],
    #                         group = charadriiformes$data[, "clade"])
    # test0 <- dispRity(data0, metric = c(sum, variances))
    # expect_is(test0, "dispRity")
    # expect_equal(names(test0), c("matrix", "tree", "call", "subsets", "disparity"))
    # expect_equal(c(summary(test0)$obs), c(0.273, 0.351, 0.360))

    # ## Same but selecting only the three first random terms
    # data <- MCMCglmm.subsets(data       = charadriiformes$data,
    #                          posteriors = charadriiformes$posteriors,
    #                          tree       = charadriiformes$tree,
    #                          group      = MCMCglmm.levels(
    #                                  charadriiformes$posteriors)[1:3],
    #                          rename.groups = c("gulls", "plovers", "sandpipers"))

    # ## The subsets are the same
    # for(i in 1:3) {
    #     expect_equal(c(data0$subsets[[i]]$elements), c(data$subsets[[i]]$elements))
    # }

    # ## Test works OK with base + covar (same as test0)
    # test1 <- dispRity(data, metric = c(sum, variances), dimensions = c(1:17))
    # expect_is(test1, "dispRity")
    # expect_equal(names(test1), c("matrix", "tree", "call", "subsets", "covar", "disparity"))
    # expect_equal(c(summary(test1)$obs), c(0.273, 0.351, 0.360))

    # ## Test works in 2 times
    # testA <- dispRity(data, metric = variances, dimensions = c(1:17))
    # expect_is(testA, "dispRity")
    # expect_equal(names(testA), c("matrix", "tree", "call", "subsets", "covar", "disparity"))
    # testB <- dispRity(testA, metric = sum)
    # expect_is(testB, "dispRity")
    # expect_equal(names(testB), c("matrix", "tree", "call", "subsets", "covar", "disparity"))
    # expect_equal(c(summary(testB)$obs), c(0.273, 0.351, 0.360))

    # ## Test works OK with as.covar
    # test2 <- dispRity(data, metric = c(sum, as.covar(variances)))
    # expect_is(test2, "dispRity")
    # expect_equal(names(test2), c("matrix", "tree", "call", "subsets", "covar", "disparity"))
    # ## Different results
    # expect_equal(c(summary(test2)$obs), c(0,0,0))



    # ## Test works in 2 times (1st covar)
    # testA <- dispRity(data, metric = as.covar(variances), dimensions = c(1:17))
    # expect_is(testA, "dispRity")
    # expect_equal(names(testA), c("matrix", "tree", "call", "subsets", "covar", "disparity"))
    # ## Works with level 1
    # testB <- dispRity(testA, metric = sum)
    # expect_is(testB, "dispRity")
    # expect_equal(names(testB), c("matrix", "tree", "call", "subsets", "covar", "disparity"))
    # expect_equal(c(summary(testB)$obs), c(0.273, 0.351, 0.360))
    # ## Error if level 1 is also covar
    # error <- capture_error(dispRity(testA, metric = as.covar(sum)))
    # expect_equal(error[[1]], "Impossible to ...")









    # ## Test works with extra arguments
    # test1 <- dispRity(data, metric = c(sum, as.covar(centroids)))
    # test2 <- dispRity(data, metric = c(sum, as.covar(centroids)), centre = 100)
    # expect_is(test1, "dispRity")
    # expect_is(test2, "dispRity")
    # expect_equal(names(test1), c("matrix", "tree", "call", "subsets", "covar", "disparity"))
    # expect_equal(names(test2), c("matrix", "tree", "call", "subsets", "covar", "disparity"))
    # ## Different results
    # expect_equal(c(summary(test1)$obs), c(0,0,0))
    # expect_equal(c(summary(test2)$obs), c(0,0,0))

})
