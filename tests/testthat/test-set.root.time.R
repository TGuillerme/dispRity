test_that("set.root.time works", {
    set.seed(1)
    ## A random tree
    my_tree <- rtree(10)
    expect_null(my_tree$root.time)
    ## Add a root time
    root_timed <- set.root.time(my_tree)
    expect_equal_round(root_timed$root.time, 3.01108, digits = 5)
    ## Add a root time with a different present
    root_timed <- set.root.time(my_tree, present = 10)
    expect_equal_round(root_timed$root.time, 13.01108, digits = 5)

    ## Works with a multiPhylo
    my_trees <- rmtree(10, 10)
    expect_true(all(unlist(lapply(my_trees, function(x) is.null(x$root.time)))))
    test <- set.root.time(my_trees)
    expect_false(all(unlist(lapply(test, function(x) is.null(x$root.time)))))

    ## Works with dispRity
    data(disparity)
    disparity$tree[[1]]$root.time <- NULL
    expect_null(disparity$tree[[1]]$root.time)
    test <- set.root.time(disparity)
    expect_equal_round(test$tree[[1]]$root.time, 139.0743, digits = 4)
})