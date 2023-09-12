test_that("sanitizing works", {

    ## Simple examples
    data(BeckLee_tree)
    data(BeckLee_mat99)
    data(BeckLee_mat50)
    data(BeckLee_ages)
    nonode_tree <- BeckLee_tree
    nonode_tree$node.labels <- NULL

    ## Base examples
    disparity_base <- dispRity(BeckLee_mat50, metric = centroids)
    disparity_group <- dispRity(custom.subsets(BeckLee_mat50, group = crown.stem(BeckLee_tree, inc.nodes = FALSE)), tree = nonode_tree, metric = centroids)
    expect_warning(disparity_time <- dispRity(chrono.subsets(BeckLee_mat50, FADLAD = BeckLee_ages, tree = BeckLee_tree, time = 5, method = "discrete"), metric = centroids, tree = nonode_tree))

    ## Sanitizing
    ## data
    wrong_dimensions <- dispRity(BeckLee_mat50, metric = c(sum, variances))
    error <- capture_error(pgls.dispRity(data = "wrong_dimensions", tree = BeckLee_tree))
    expect_equal(error[[1]], "data must be of class dispRity.")
    error <- capture_error(pgls.dispRity(data = wrong_dimensions, tree = BeckLee_tree))
    expect_equal(error[[1]], "Impossible to run a univariate pgls on wrong_dimensions because doesn't contain a dimension level-2 metric. See ?dispRity.metric for more info.")
    
    ## tree
    wrong_tree <- rtree(50)
    error <- capture_error(pgls.dispRity(data = disparity_base, tree = "wrong_tree"))
    expect_equal(error[[1]], "tree must be of class phylo or multiPhylo.")
    expect_warning(error <- capture_error(pgls.dispRity(data = disparity_base, tree = wrong_tree)))
    expect_equal(error[[1]], "The data is not matching the tree labels (you can use ?clean.data to match both data and tree).")

    ## formula
    wrong_formula <- dispoority ~ 1
    error <- capture_error(pgls.dispRity(data = disparity_base, tree = nonode_tree, formula = wrong_formula))
    expect_equal(error[[1]], "The response term of the formula must be 'disparity'.")
    expect_equal(get.formula(disparity_base), disparity ~ 1)
    expect_equal(get.formula(disparity_group), disparity ~ group)
    error <- capture_error(get.formula(disparity_time))
    expect_equal(error[[1]], "Some groups have overlapping elements.")
    disparity_time2 <- dispRity(chrono.subsets(BeckLee_mat50, tree = BeckLee_tree, time = c(140, 66, 0), method = "discrete"), metric = centroids, tree = nonode_tree)
    error <- capture_error(get.formula(disparity_time2))
    expect_equal(error[[1]], "It is currently not possible to apply an phylogenetic linear model on dispRity data with time series.")

    ## model
    error <- capture_error(pgls.dispRity(data = disparity_base, tree = nonode_tree, model = "SM"))
    expect_equal(error[[1]], "model must be one of the following: BM, OUrandomRoot, OUfixedRoot, lambda, kappa, delta, EB, trend.")

    ## Testing super simple
    test <- pgls.dispRity(disparity_base, tree = nonode_tree)
    expect_is(test, "phylolm")
    expect_equal(test$call, "dispRity interface of phylolm using: formula = disparity ~ 1 and model = BM")

    ## Testing group example
    test <- pgls.dispRity(disparity_group, tree = nonode_tree)
    expect_is(test, "phylolm")
    expect_equal(test$call, "dispRity interface of phylolm using: formula = disparity ~ group and model = BM")

    ## Wrapped up test
    disparity <- dispRity(BeckLee_mat50, metric = centroids, tree = BeckLee_tree)
    test <- test.dispRity(disparity, test = pgls.dispRity)
    expect_is(test, "phylolm")
    expect_equal(test$call, "dispRity interface of phylolm using: formula = disparity ~ 1 and model = BM")




    # ## Multiple trees example
    # set.seed(1)
    # trees <- replicate(3, rcoal(10), simplify = FALSE)
    # class(trees) <- "multiPhylo"
    # disparities <- replicate(3, matrix(rnorm(30), ncol = 3, dimnames = list(paste0("t", 1:10))), simplify = FALSE)

    # ## Multiple matrices example

    # ## Multiple trees and matrices

    # ## Checks whether the dispRity object contains a tree or whether a tree can be added

    # ## Check whether the dispRity object is a level 2 metric at least

    # ## Check if formula is interpretable

    # ## Check arguments for phylolm



})